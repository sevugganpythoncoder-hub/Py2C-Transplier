import ast
import os
import sys
import subprocess
import tkinter as tk
from tkinter import ttk
import traceback
import multiprocessing

class Eng(ast.NodeVisitor):
    def __init__(self, m="c"):
        self.m = m.lower()
        self.buf = []
        self.ind = 1
        self.vmap = {}
        self.funcs = []

    def pad(self):
        return "    " * self.ind

    def inf(self, n):
        if isinstance(n, ast.Constant):
            if isinstance(n.value, str): return "string"
            if isinstance(n.value, float): return "double"
            if isinstance(n.value, bool): return "bool"
            if isinstance(n.value, int): return "int"
        elif isinstance(n, (ast.JoinedStr, ast.Call)):
            if isinstance(n, ast.Call) and getattr(n.func, 'id', '') == 'input':
                return "string"
            return "string" if isinstance(n, ast.JoinedStr) else "int"
        elif isinstance(n, ast.List):
            return "list"
        elif isinstance(n, ast.Name):
            return self.vmap.get(n.id, "int")
        elif isinstance(n, ast.BinOp):
            l, r = self.inf(n.left), self.inf(n.right)
            if l == "double" or r == "double" or isinstance(n.op, ast.Div):
                return "double"
            return "int"
        elif isinstance(n, ast.BoolOp):
            return "bool"
        return "int"

    def exp(self, n):
        if isinstance(n, ast.Constant):
            if isinstance(n.value, str): return f'"{n.value}"'
            if isinstance(n.value, bool): return "true" if n.value else "false"
            return str(n.value)
        elif isinstance(n, ast.Name):
            return n.id
        elif isinstance(n, ast.JoinedStr):
            parts = []
            for val in n.values:
                if isinstance(val, ast.FormattedValue): parts.append(self.exp(val.value))
                elif isinstance(val, ast.Constant): parts.append(f'"{val.value}"')
            return " + ".join(parts) if self.m == "cpp" else (parts[0] if parts else '""')
        elif isinstance(n, ast.BinOp):
            l, r = self.exp(n.left), self.exp(n.right)
            ops = {ast.Add: "+", ast.Sub: "-", ast.Mult: "*", ast.Div: "/", ast.Mod: "%"}
            return f"({l} {ops.get(type(n.op), '+')} {r})"
        elif isinstance(n, ast.BoolOp):
            op_str = " && " if isinstance(n.op, ast.And) else " || "
            values = [self.exp(v) for v in n.values]
            return f"({op_str.join(values)})"
        elif isinstance(n, ast.UnaryOp) and isinstance(n.op, ast.Not):
            return f"(!{self.exp(n.operand)})"
        elif isinstance(n, ast.Compare):
            left = self.exp(n.left)
            ops_map = {ast.Eq: "==", ast.NotEq: "!=", ast.Lt: "<", ast.LtE: "<=", ast.Gt: ">", ast.GtE: ">="}
            op = ops_map.get(type(n.ops[0]), "==")
            right = self.exp(n.comparators[0])
            
            if self.m == "c" and (self.inf(n.left) == "string" or self.inf(n.comparators[0]) == "string"):
                if op == "==":
                    return f"(strcmp({left}, {right}) == 0)"
                elif op == "!=":
                    return f"(strcmp({left}, {right}) != 0)"
            return f"({left} {op} {right})"
        elif isinstance(n, ast.Call):
            fname = getattr(n.func, 'id', '')
            args = [self.exp(a) for a in n.args]
            if fname == "input":
                return '""'
            return f"{fname}({', '.join(args)})"
        return "0"

    def visit_Module(self, n):
        body_nodes = []
        for stmt in n.body:
            if isinstance(stmt, ast.FunctionDef):
                self.visit_FunctionDef(stmt)
            else:
                body_nodes.append(stmt)

        if self.m == "cpp":
            self.buf.append("#include <iostream>\n#include <string>\n#include <vector>\nusing namespace std;\n")
        else:
            self.buf.append("#include <stdio.h>\n#include <stdbool.h>\n#include <string.h>\n#include <stdlib.h>\n")
        
        if self.funcs:
            self.buf.extend(self.funcs)
            self.buf.append("")

        self.buf.append("int main() {")
        for stmt in body_nodes:
            self.visit(stmt)
        self.buf.append(f"{self.pad()}return 0;\n}}")
        return "\n".join(self.buf)

    def visit_FunctionDef(self, n):
        old_ind = self.ind
        self.ind = 1
        
        ret_type = "int"
        for stmt in n.body:
            if isinstance(stmt, ast.Return) and stmt.value:
                inf_t = self.inf(stmt.value)
                if inf_t == "double": ret_type = "double"
                elif inf_t == "string": ret_type = "string" if self.m == "cpp" else "const char*"
                break

        args = [f"int {a.arg}" for a in n.args.args]
        f_code = [f"{ret_type} {n.name}({', '.join(args)}) {{"]
        for stmt in n.body:
            if isinstance(stmt, ast.Return):
                f_code.append(f"    return {self.exp(stmt.value)};")
            else:
                self.ind = 1
                tmp_buf = self.buf
                self.buf = []
                self.visit(stmt)
                f_code.extend(self.buf)
                self.buf = tmp_buf
        f_code.append("}")
        self.funcs.append("\n".join(f_code))
        self.ind = old_ind

    def visit_Assign(self, n):
        v = n.targets[0].id
        val_node = n.value
        
        if isinstance(val_node, ast.Call) and getattr(val_node.func, 'id', '') == 'input':
            prompt = self.exp(val_node.args[0]) if val_node.args else ""
            if self.m == "cpp":
                if prompt: self.buf.append(f"{self.pad()}cout << {prompt} << flush;")
                self.buf.append(f"{self.pad()}string {v};\n{self.pad()}if(cin.peek() == '\\n') cin.ignore();\n{self.pad()}getline(cin, {v});\n{self.pad()}if (!{v}.empty() && {v}.back() == '\\r') {v}.pop_back();")
            else:
                if prompt: self.buf.append(f"{self.pad()}printf(\"%s\", {prompt});")
                self.buf.append(f"{self.pad()}char {v}[256];\n{self.pad()}fgets({v}, sizeof({v}), stdin);\n{self.pad()}{v}[strcspn({v}, \"\\r\\n\")] = 0;")
            self.vmap[v] = "string"
            return

        val = self.exp(val_node)
        t = self.inf(val_node)
        decl = "string" if t == "string" and self.m == "cpp" else ("const char*" if t == "string" else ("double" if t == "double" else ("bool" if t == "bool" else ("vector<int>" if t == "list" and self.m == "cpp" else "int"))))

        if v not in self.vmap:
            self.vmap[v] = t
            if t == "list" and isinstance(val_node, ast.List):
                elts = [self.exp(e) for e in val_node.elts]
                if self.m == "cpp":
                    self.buf.append(f"{self.pad()}vector<int> {v} = {{{', '.join(elts)}}};")
                else:
                    self.buf.append(f"{self.pad()}int {v}[] = {{{', '.join(elts)}}};")
            else:
                self.buf.append(f"{self.pad()}{decl} {v} = {val};")
        else:
            self.buf.append(f"{self.pad()}{v} = {val};")

    def visit_If(self, n):
        cond = self.exp(n.test)
        self.buf.append(f"{self.pad()}if ({cond}) {{")
        self.ind += 1
        for s in n.body: self.visit(s)
        self.ind -= 1
        if n.orelse:
            self.buf.append(f"{self.pad()}}} else {{")
            self.ind += 1
            for s in n.orelse: self.visit(s)
            self.ind -= 1
        self.buf.append(f"{self.pad()}}}")

    def visit_While(self, n):
        cond = self.exp(n.test)
        self.buf.append(f"{self.pad()}while ({cond}) {{")
        self.ind += 1
        for s in n.body: self.visit(s)
        self.ind -= 1
        self.buf.append(f"{self.pad()}}}")

    def visit_For(self, n):
        v = n.target.id
        lim = self.exp(n.iter.args[0]) if isinstance(n.iter, ast.Call) and getattr(n.iter.func, 'id', '') == 'range' else "0"
        inc = f"++{v}" if self.m == "cpp" else f"{v}++"
        self.buf.append(f"{self.pad()}for (int {v} = 0; {v} < {lim}; {inc}) {{")
        self.ind += 1
        for s in n.body: self.visit(s)
        self.ind -= 1
        self.buf.append(f"{self.pad()}}}")

    def visit_Break(self, n):
        self.buf.append(f"{self.pad()}break;")

    def visit_Continue(self, n):
        self.buf.append(f"{self.pad()}continue;")

    def visit_Pass(self, n):
        self.buf.append(f"{self.pad()};")

    def visit_Expr(self, n):
        if isinstance(n.value, ast.Call) and getattr(n.value.func, 'id', '') == "print":
            args = n.value.args
            if not args: return
            aval = self.exp(args[0])
            atype = self.inf(args[0])
            if self.m == "cpp":
                self.buf.append(f"{self.pad()}cout << {aval} << endl;")
            else:
                fmt = "%s" if atype in ["string", "bool"] else ("%f" if atype == "double" else "%d")
                if atype == "bool": aval = f"({aval} ? \"true\" : \"false\")"
                self.buf.append(f'{self.pad()}printf("{fmt}\\n", {aval});')

class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("MicroPy2C v1.2")
        self.geometry("1000x700")
        self.configure(bg="#11111b")

        bar = tk.Frame(self, bg="#1e1e2e", height=40)
        bar.pack(fill="x", padx=5, pady=5)

        tk.Label(bar, text="Target:", fg="#cdd6f4", bg="#1e1e2e", font=("Consolas", 10, "bold")).pack(side="left", padx=5)
        self.lvar = tk.StringVar(value="C++")
        drop = ttk.Combobox(bar, textvariable=self.lvar, values=["C", "C++"], state="readonly", width=8)
        drop.pack(side="left", padx=5)
        drop.bind("<<ComboboxSelected>>", lambda e: self.chk())

        btn = tk.Button(bar, text="Run Native", command=self.go, bg="#a6e3a1", fg="#11111b", font=("Consolas", 10, "bold"), relief="flat", padx=10)
        btn.pack(side="left", padx=15)

        self.lbl = tk.Label(bar, text="Status: Ready", fg="#a6e3a1", bg="#1e1e2e", font=("Consolas", 10))
        self.lbl.pack(side="right", padx=10)

        p1 = tk.PanedWindow(self, orient="vertical", bg="#313244", bd=0)
        p1.pack(fill="both", expand=True, padx=5, pady=5)
        p2 = tk.PanedWindow(p1, orient="horizontal", bg="#313244", bd=0)

        f1 = tk.Frame(p2, bg="#181825")
        tk.Label(f1, text="Python Input", fg="#f5c2e7", bg="#181825", font=("Consolas", 10, "bold")).pack(anchor="w", padx=5, pady=2)
        self.t1 = tk.Text(f1, bg="#11111b", fg="#a6e3a1", insertbackground="white", font=("Consolas", 11), bd=0, undo=True)
        self.t1.pack(fill="both", expand=True)
        self.t1.bind("<KeyRelease>", lambda e: self.chk())
        self.t1.bind("<Return>", self.indent)
        p2.add(f1)

        f2 = tk.Frame(p2, bg="#181825")
        tk.Label(f2, text="Transpiled Output", fg="#89b4fa", bg="#181825", font=("Consolas", 10, "bold")).pack(anchor="w", padx=5, pady=2)
        self.t2 = tk.Text(f2, bg="#11111b", fg="#89b4fa", font=("Consolas", 11), bd=0)
        self.t2.pack(fill="both", expand=True)
        p2.add(f2)

        p1.add(p2, height=450)

        f3 = tk.Frame(p1, bg="#181825")
        tk.Label(f3, text="Console Output", fg="#f9e2af", bg="#181825", font=("Consolas", 10, "bold")).pack(anchor="w", padx=5, pady=2)
        self.t3 = tk.Text(f3, bg="#11111b", fg="#f9e2af", font=("Consolas", 10), bd=0)
        self.t3.pack(fill="both", expand=True)
        p1.add(f3)

        code = 'user_input = input("Continue? (y/n): ")\nif user_input == "y":\n    print("Continuing...")\nelse:\n    print("Exiting...")'
        self.t1.insert("1.0", code)
        self.chk()

    def indent(self, event):
        curr_line_before_cursor = self.t1.get("insert linestart", "insert")
        pad = "".join(char for char in curr_line_before_cursor if char in (" ", "\t"))
        if curr_line_before_cursor.strip().endswith(":"):
            pad += "    "
        self.t1.insert("insert", "\n" + pad)
        return "break"

    def set_out(self, txt, col="#89b4fa"):
        self.t2.config(state="normal", fg=col)
        self.t2.delete("1.0", tk.END)
        self.t2.insert("1.0", txt)
        self.t2.config(state="disabled")

    def set_log(self, txt, col="#f9e2af"):
        self.t3.config(state="normal", fg=col)
        self.t3.delete("1.0", tk.END)
        self.t3.insert("1.0", txt)
        self.t3.config(state="disabled")

    def chk(self):
        src = self.t1.get("1.0", tk.END).strip()
        if not src:
            self.set_out("")
            return

        try:
            tree = ast.parse(src)
            e = Eng(m=self.lvar.get())
            self.last = e.visit(tree)
            self.set_out(self.last, col="#89b4fa")
            self.lbl.config(text="Status: OK", fg="#a6e3a1")
        except Exception as err:
            msg = f"Error: {err}\n\n" + "".join(traceback.format_exception(type(err), err, err.__traceback__))
            self.set_out(msg, col="#f38ba8")
            self.lbl.config(text="Status: Syntax Error", fg="#f38ba8")

    def go(self):
        m = self.lvar.get().lower()
        src = getattr(self, 'last', '')
        if not src:
            self.set_log("No code to run", col="#f38ba8")
            return

        ext = "cpp" if m == "cpp" else "c"
        src_f = os.path.abspath(f"temp_build.{ext}")
        exe_f = os.path.abspath("temp_build.exe" if os.name == "nt" else "./temp_build")

        with open(src_f, "w", encoding="utf-8") as f:
            f.write(src)

        if getattr(sys, 'frozen', False):
            root = sys._MEIPASS
        else:
            root = os.getcwd()
            
        tdir = os.path.join(root, "tcc")
        tbin = os.path.join(tdir, "tcc.exe")
        
        cc = None
        env = os.environ.copy()
        compile_flags = subprocess.CREATE_NO_WINDOW if os.name == "nt" else 0

        if m == "c" and os.path.exists(tbin):
            cc = tbin
        else:
            candidates = ["tcc", "gcc", "clang"] if m == "c" else ["g++", "clang++"]
            for c in candidates:
                try:
                    subprocess.run([c, "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.DEVNULL, creationflags=compile_flags, env=env)
                    cc = c
                    break
                except FileNotFoundError:
                    continue

        if not cc:
            self.set_log("No C/C++ compiler found", col="#f38ba8")
            return

        try:
            if cc == tbin:
                compile_cmd = [cc, f"-B{tdir}", f"-I{os.path.join(tdir, 'include')}", f"-L{os.path.join(tdir, 'lib')}", src_f, "-o", exe_f]
            else:
                compile_cmd = [cc, "-O2", src_f, "-o", exe_f]

            cres = subprocess.run(compile_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.DEVNULL, text=True, creationflags=compile_flags, env=env)
            if cres.returncode != 0:
                self.set_log(f"Compilation Error:\n{cres.stderr}", col="#f38ba8")
                return

            if os.name == "nt":
                subprocess.Popen(f'start cmd /k "{exe_f} & pause & exit"', shell=True)
            else:
                subprocess.Popen([exe_f])
            
            self.set_log("Running in interactive console window...", col="#a6e3a1")
        except subprocess.TimeoutExpired:
            self.set_log("Execution timed out.", col="#f38ba8")
        except Exception as e:
            self.set_log(f"Error: {e}", col="#f38ba8")
        finally:
            if os.path.exists(src_f):
                try: os.remove(src_f)
                except OSError: pass

if __name__ == "__main__":
    multiprocessing.freeze_support()
    App().mainloop()
