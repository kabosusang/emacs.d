# 我的 Emacs 配置

Forked from [Pavinberg/emacs.d](https://github.com/Pavinberg/emacs.d)

一套为 Python/Rust/C++ 开发优化的 Emacs 配置。  

使用 `lsp-bridge` 提供极速流畅的代码补全体验，并集成了 `claude-code` AI 辅助编程。  

如果要使用`lsp-mode` 移步[lsp-mode](https://github.com/kabosusang/emacs.d/tree/main)


## 特性

- ⚡ **lsp-bridge**：异步 LSP 补全
- 🐍 **Python**：`pyright` + `ruff`
- 🦀 **Rust**：`rust-analyzer` 
- 🔧 **C/C++**：`clangd` + `flycheck-clang-tidy`
- 🔮 **Scheme**：`guile` + `paredit` + `geiser`
- 🤖 **Claude Code**：AI 辅助编程集成
- 🎨 现代化界面：`ivy`/`counsel`、`dashboard`、`treemacs`
- ✨ 多光标编辑：`multiple-cursors` + `hydra`
- 📦 Git 集成：`magit` 

## 安装

### 克隆仓库
```bash
git clone --recursive https://github.com/kabosu/.emacs.d.git ~/.emacs.d
```
### 创建 Python 虚拟环境
LSP 服务器和 lsp-bridge 依赖需要安装在专用的虚拟环境中：  
```bash
# 创建虚拟环境
uv venv ~/.emacs.d/.venv-emacs --python 3.11

# 安装 LSP Python服务器
uv pip install --python ~/.emacs.d/.venv-emacs/bin/python pyright ruff

# 安装 lsp-bridge 依赖
uv pip install --python ~/.emacs.d/.venv-emacs/bin/python epc orjson rapidfuzz watchdog six packaging
```
### Python依赖如下
```baseh
╰─λ uv pip list --python ~/.emacs.d/.venv-emacs/bin/python                                         2 (0.006s) < 22:31:45
Using Python 3.11.15 environment at: /home/kabosu/.emacs.d/.venv-emacs
Package               Version
--------------------- -------
bcrypt                5.0.0
cffi                  2.0.0
cryptography          46.0.7
epc                   0.0.5
invoke                3.0.3
nodeenv               1.10.0
nodejs-wheel-binaries 24.14.1
orjson                3.11.8
paramiko              4.0.0
pycparser             3.0
pynacl                1.6.2
pyright               1.1.408
rapidfuzz             3.14.5
ruff                  0.15.10
setuptools            82.0.1
sexpdata              1.0.2
six                   1.17.0
typing-extensions     4.15.0
watchdog              6.0.0
```

## 启动
首次启动`emacs`可能需要安装
```bash
M-x package-install vterm
```

## ScreenShot
![](screenshot/book.png)


## 注意  
如果要使用Python编程需要切换虚拟环境 比如项目目录有.venv虚拟环境 添加`lsp-bridge`的环境  
之后执行`my/setup-uv-virtualenv` 切换虚拟环境 需要给项目安装`lsp-bridge`依赖
```base
uv pip install --python .venv/bin/python pyright ruff
uv pip install --python .venv/bin/python epc orjson rapidfuzz watchdog six packaging 
```

