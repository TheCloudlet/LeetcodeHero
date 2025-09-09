# RISC-V 編譯器後端面試手冊 v2.0 (通用版)

> **目標:** RISC-V 編譯器後端工程師面試
> **核心戰略:** 專注於後端三大支柱：ISel, RegAlloc, Scheduling。
> **JD 關鍵字:** [LLVM/Optimization], [RISC-V], [Performance Issues], [Profiling Data], [Assembly Code]

---

## 1. SoC 設計公司為何需要專門的 LLVM 後端團隊？

一個常見的疑問是：既然 RISC-V 基金會和開源社群會維護一個「通用」的後端到 LLVM 主幹 (upstream)，為什麼一家 SoC 設計公司還需要專門的編譯器團隊？

答案是：開源後端是為「**通用硬體**」寫的。而一家頂級的 SoC 公司的核心業務是設計「**專用硬體**」，他們必須在效能和功耗上擊敗競爭對手。他們雇用編譯器後端工程師的目的，是去做開源社群無法做的事：

### A. 微架構調優 (Micro-architectural Tuning)

開源的通用後端不知道這家公司*實際上*是如何實現 RISC-V 規格的。它不知道：
* **管線 (Pipeline) 深度**：這顆 CPU 的管線有幾級？
* **指令延遲 (Instruction Latency)**：乘法器需要 2 週期還是 5 週期？
* **執行單元 (Execution Units)**：有幾個 ALU？幾個 Load/Store 單元？

**工程師職責 (對應 Day 4 - Scheduling):**
必須為該公司的特定微架構，提供一個精確的「**排程模型 (Scheduling Model)**」和「**成本模型 (Cost Model)**」。必須「教」LLVM 的指令排程器，如何重新排序組合語言，才能完美填滿自家硬體的管線，避免週期浪費 (Stalls)。

### B. 客製化指令集擴展 (Custom ISA Extensions) - 【核心價值】

RISC-V 的美妙之處在於它是「可擴展的」。一家 SoC 公司可以在標準指令集之外，增加他們「**自己的私有指令**」。

**工程師職責 (對應 Day 2 - ISel):**
當硬體團隊為 AI/5G 演算法設計了一條私有指令（例如一條向量融合乘加指令），一個週期能做完 10 條通用指令的工作。但是，開源的 LLVM 後端不認識這條指令。
工程師的工作就是：
1.  **修改 `TableGen` (`.td` 檔):** 定義這條新指令的規格。
2.  **編寫 ISel Pattern:** 告訴 LLVM：「當你在 IR 中看到『這種』數學運算模式時，**必須**把它匹配並替換成我們這條超快的『私有指令』！」

---

## 2. RISC-V 指令集速成 (The Philosophy & The Rules)

### A. RISC 哲學：Load-Store 架構

* **RISC (精簡指令集):** 指令長度固定 (32-bit)，設計極簡、正交。
* **CISC (複雜指令集):** (如 x86) 指令長度可變，一條指令可以做很複雜的事 (如 `add [mem], reg`)。
* **RISC 核心規則:**
    1.  **運算只在暫存器:** 所有計算 (如 `add`, `sub`) **只能**在暫存器 (Registers) 之間發生。
    2.  **記憶體存取分離:** **只有** `load` 和 `store` 兩種指令可以碰記憶體。

### B. 指令格式 (Instruction Formats)

所有基礎 RISC-V 指令都是 32 位元寬。指令的格式決定了這 32 個位元如何被劃分給 opcode、暫存器編號、立即數等。

* **R-Type (Register-Register):** 用於三個暫存器之間的運算。
    * `| funct7 | rs2 | rs1 | funct3 | rd | opcode |`
    * 範例: `add rd, rs1, rs2` (rd = rs1 + rs2)

* **I-Type (Immediate):** 用於暫存器和一個短立即數之間的運算。
    * `| imm[11:0] | rs1 | funct3 | rd | opcode |`
    * 範例: `addi rd, rs1, imm` (rd = rs1 + imm)
    * 範例: `lw rd, offset(rs1)` (rd = mem[rs1 + offset])

* **S-Type (Store):** 用於儲存一個暫存器的值到記憶體。
    * `| imm[11:5] | rs2 | rs1 | funct3 | imm[4:0] | opcode |`
    * 範例: `sw rs2, offset(rs1)` (mem[rs1 + offset] = rs2)

* **B-Type (Branch):** 用於條件分支。
    * `| imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode |`
    * 範例: `beq rs1, rs2, label` (if (rs1 == rs2) goto label)

### C. ABI 呼叫慣例 (The Social Contract)

這是編譯器必須遵守的「社會契約」，規定了函式之間如何使用共享的暫存器。這也是解釋不同優化等級之間組合語言差異的關鍵。

| ABI 名稱         | 角色（用途）                     | 儲存規則            | 責任方                                                                                             |
| :--------------- | :------------------------------- | :------------------ | :------------------------------------------------------------------------------------------------- |
| `a0`-`a7`        | **Arguments** / **Return**       | **Caller-Saved**    | **被呼叫者 (Callee)** 可以隨意摧毀。呼叫者 (Caller) 自己有責任在呼叫前保存。                       |
| `t0`-`t6`        | **Temporary** (臨時)             | **Caller-Saved**    | 同上。                                                                                             |
| `ra`             | **Return Address**               | **Caller-Saved**    | 同上。                                                                                             |
| `s0`-`s11`       | **Saved Registers** (保留)       | **Callee-Saved**    | **被呼叫者 (Callee)** *必須*保證這些暫存器在返回時完好如初。如果非用不可，必須自己負責保存和恢復。 |
| `sp`, `gp`, `tp` | **Stack/Global/Thread Pointers** | Callee-Saved (特殊) | 由 Callee 維護。                                                                                   |

### D. 優化等級的視覺差異 (Godbolt 實驗)

我們觀想 `add5(int x)` 函式時，看到了優化器如何利用 ABI 規則。

**在 `-O0` (Debug 模式)，編譯器產生了冗長的堆疊操作：**

```asm
add5(int):
        addi    sp, sp, -16
        sw      ra, 12(sp)
        sw      s0, 8(sp)     ; <--- 儲存 Callee-Saved 暫存器
        addi    s0, sp, 16
        sw      a0, -12(s0)   ; <--- 變數溢出 (Spill) 到堆疊
        lw      a0, -12(s0)   ; <--- 從堆疊重新載入 (Reload)
        addi    a0, a0, 5     ; <--- *本質工作*
        lw      ra, 12(sp)
        lw      s0, 8(sp)     ; <--- 恢復 Callee-Saved 暫存器
        addi    sp, sp, 16
        ret
```