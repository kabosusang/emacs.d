;;; init-lsp-bridge-test.el --- Test configuration for lsp-bridge
;;; Commentary:
;;; This file helps verify that lsp-bridge is working correctly.

;;; Code:

;; Create test Python file
(with-temp-file "/tmp/test_lsp_bridge.py"
  (insert "import numpy as np\n")
  (insert "from typing import List\n\n")
  (insert "class TestClass:\n")
  (insert "    def __init__(self, value: int):\n")
  (insert "        self.value = value\n")
  (insert "        self.items = [1, 2, 3]\n\n")
  (insert "    def get_items(self) -> List[int]:\n")
  (insert "        return self.items\n\n")
  (insert "if __name__ == \"__main__\":\n")
  (insert "    test = TestClass(42)\n")
  (insert "    print(f\"Value: {test.value}\")\n"))

;; Create test C file
(with-temp-file "/tmp/test_lsp_bridge.c"
  (insert "#include <stdio.h>\n")
  (insert "#include <stdlib.h>\n\n")
  (insert "int main() {\n")
  (insert "    printf(\"Hello World\\n\");\n")
  (insert "    return 0;\n")
  (insert "}\n"))

;; Create test Rust file
(with-temp-file "/tmp/test_lsp_bridge.rs"
  (insert "fn main() {\n")
  (insert "    let vec = vec![1, 2, 3];\n")
  (insert "    println!(\"{:?}\", vec);\n")
  (insert "}\n"))

(provide 'init-lsp-bridge-test)
;;; init-lsp-bridge-test.el ends here