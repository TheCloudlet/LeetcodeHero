# RISC-V 編譯器後端面試手冊 v3.0 (整合版)

- **面試目標:** RISC-V Compiler Backend Engineer
- **核心戰略:** 掌握後端三大支柱 - ISel、RegAlloc、Scheduling
- **JD 關鍵字:** `LLVM/Optimization` | `RISC-V` | `Performance Issues` | `Profiling Data` | `Assembly Code`

______________________________________________________________________

## 1. 為什麼 SoC 設計公司需要專門的 LLVM Backend 團隊？

### \[Q\] 常見疑問

既然 RISC-V Foundation 和開源社群會維護一個「通用」的 backend 到 LLVM upstream，為什麼 SoC 設計公司還需要專門的 compiler 團隊？

### \[A\] 核心答案

- **開源 backend** -> 針對「**通用硬體 (Generic Hardware)**」設計
- **頂級 SoC 公司** -> 核心業務是設計「**專用硬體 (Specialized Hardware)**」
- **目標** -> 在效能 (Performance) 和功耗 (Power) 上擊敗競爭對手

**編譯器後端工程師的使命：做開源社群無法做的事**

### A. 微架構調優 (Micro-architectural Tuning)

開源的通用後端不知道這家公司*實際上*是如何實現 RISC-V 規格的。它不知道：

- **管線 (Pipeline) 深度**：這顆 CPU 的管線有幾級？
- **指令延遲 (Instruction Latency)**：乘法器需要 2 週期還是 5 週期？
- **執行單元 (Execution Units)**：有幾個 ALU？幾個 Load/Store 單元？

**\[職責\] 工程師職責:**

- 提供精確的「**排程模型 (Scheduling Model)**」和「**成本模型 (Cost Model)**」來「教」LLVM 如何為自家硬體產生最佳程式碼。

**\[實作\] 效能的真理來源：`TargetSubtarget` C++ 檔案**

- **Q:** 記錄 Cycle 數和硬體規格的檔案在哪？
- **A:** 它不在 `.td` 檔。它在一個核心的 **C++ 物件**裡：**`TargetSubtarget`** (例如 `llvm/lib/Target/RISCV/RISCVSubtarget.cpp`)。
- **這是什麼:** 這是目標硬體的「**數位靈魂**」。它用 C++ 程式碼定義了：
  - **指令延遲 (Instruction Latencies):** `add` 幾週期，`div` 幾週期。
  - **執行單元 (Execution Units):** 有幾個 ALU，幾個 FPU。
  - **分支預測懲罰 (Branch Mispredict Penalty):** Branch miss 的成本。
- **你的工作:** 開源的 `RISCVSubtarget.cpp` 是通用猜測值。你的工作，就是為公司的晶片，**手動填寫**經過硬體團隊測量出來的、**作為商業機密的、真實準確的週期數字**。這個檔案是 ISel、RegAlloc、Scheduler 所有成本模型和決策的**唯一真理來源**。

### B. 客製化指令集擴展 (Custom ISA Extensions)

RISC-V 的美妙之處在於它是「**可擴展的 (Extensible)**」。SoC 公司可以在標準指令集之外，增加他們「**私有指令 (Private Instructions)**」。

**\[職責\] 工程師職責:**

1. **修改 TableGen (`.td` 檔案)**：定義新私有指令的規格。
1. **編寫 ISel Pattern**：告訴 LLVM「當在 IR 中看到『這種』數學運算模式時，**必須**匹配並替換成我們的超快『私有指令』」。

______________________________________________________________________

## 2. RISC-V 指令集速成 (The Philosophy & The Rules)

### A. RISC 哲學：Load-Store 架構

**核心規則:**

- **運算只在暫存器** -> 所有計算 (如 `add`, `sub`) **只能**在 registers 之間發生。
- **記憶體存取分離** -> **只有** `load` 和 `store` 兩種指令可以碰記憶體。

### B. 指令格式 (Instruction Formats)

所有基礎 RISC-V 指令都是 32 位元寬。

- **R-Type (Register-Register):** 三個暫存器間的運算 (`add rd, rs1, rs2`)。
  - `| funct7 | rs2 | rs1 | funct3 | rd | opcode |`
- **I-Type (Immediate):** Register 和短立即數運算 (`addi rd, rs1, imm`, `lw rd, offset(rs1)`)。
  - `| imm[11:0] | rs1 | funct3 | rd | opcode |`
- **S-Type (Store):** 儲存 register 值到記憶體 (`sw rs2, offset(rs1)`)。
  - `| imm[11:5] | rs2 | rs1 | funct3 | imm[4:0] | opcode |`
- **B-Type (Branch):** 條件分支 (`beq rs1, rs2, label`)。
  - `| imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode |`

### C. ABI 呼叫慣例 (The Social Contract)

規定了函式之間如何使用共享的暫存器。

| ABI 名稱         | 角色                             | 儲存規則            | 責任方                                                                   |
|:-----------------|:---------------------------------|:--------------------|:-------------------------------------------------------------------------|
| `a0`-`a7`        | **Arguments** / **Return**       | **Caller-Saved**    | **Callee** 可以隨意摧毀。**Caller** 自己有責任保存。                       |
| `t0`-`t6`        | **Temporary** (臨時)             | **Caller-Saved**    | 同上。                                                                    |
| `ra`             | **Return Address**               | **Caller-Saved**    | 同上。                                                                    |
| `s0`-`s11`       | **Saved Registers** (保留)       | **Callee-Saved**    | **Callee** *必須*保證返回時完好如初。如果非用不可，必須自己負責保存和恢復。 |
| `sp`, `gp`, `tp` | **Stack/Global/Thread Pointers** | Callee-Saved (特殊) | 由 Callee 維護。                                                          |

______________________________________________________________________

## 3. LLVM 後端工作流程詳解 (The How)

這份基於 LLVM 官方文件的 7 步驟流程，是後端的「原始碼真相」。

1. **Instruction Selection (ISel)**
1. **Scheduling and Formation (Pre-RA)**
1. **SSA-based Machine Code Optimizations**
1. **Register Allocation (RegAlloc)**
1. **Prolog/Epilog Insertion (PEI)**
1. **Late Machine Code Optimizations**
1. **Code Emission**

TODO: 每一個階段輸入跟輸出是什麼
TODO: 每一個階段為什麼要放在這個位置


### 步驟 1: 指令選擇 (Instruction Selection - ISel)

- **核心理論:** **DAG Pattern Matching**。ISel 將 IR 轉成一個 DAG (圖)，然後用 CPU 的「指令拼圖 (Patterns)」去「覆蓋 (Tile)」這個 DAG，找出總成本最低的方案。
- **LLVM 實作:** **TableGen (`.td` 檔)**。
  - `.td` 檔是硬體的「宣告式資料庫」。工程師在裡面「描述」硬體規則。
  - `llvm-tablegen` 工具讀取 `.td`，並**自動生成**高效的 C++ 匹配器程式碼。
- **判斷基準:** **預設是「預估的時脈週期 (Estimated Cycles)」**。在 `-Os` (優化大小) 模式下，基準會變為「指令的位元組長度」。

### 步驟 2: 排程與成形 (Pre-RA Scheduling)

- **運行時機:** ISel 之後，RegAlloc 之前。
- **雙重任務:**
  - **1. (明顯的) 隱藏硬體延遲 (Hide Latency):** 重新排序指令，將不相關的指令塞進 `load` 等長延遲指令的「空檔」中，以填滿管線。
  - **2. (精妙的) 最小化暫存器壓力 (Minimize Register Pressure):** 盡可能「**縮短**」每個虛擬暫存器的「**存活區間 (Live Range)**」。一個好的排程會讓干涉圖變得稀疏，更容易著色，從而**從根本上減少 Spilling 的機率**。

### 步驟 3: SSA-based Machine Code Optimizations

- (此為進階主題，我們尚未深入討論)

### 步驟 4: 暫存器分配 (Register Allocation - RegAlloc)

- **核心問題:** 將「無限多虛擬暫存器」映射到「有限的物理暫存器」。
- **核心理論:** **圖形著色 (Graph Coloring)**。
  - **建立干涉圖 (Interference Graph):** Live Range 是「節點」，重疊的 Live Range 之間有「邊」。
  - **K-Coloring:** 用 K 個物理暫存器（顏色）去塗這張圖。
- **危機：Spilling (溢出)**
  - **觸發:** 當干涉圖太複雜，導致圖形著色**數學上無解**。
  - **解決方案:** 挑選「**成本最低**」的犧牲者（基於迴圈深度加權的使用次數），將其從暫存器驅逐到堆疊，並自動插入昂貴的 `sw` (Spill) 和 `lw` (Reload) 指令。
- **SSA 的挑戰：消滅 Phi (PHI Elimination)**
  - **問題:** CPU 沒有 `phi` 指令，RegAlloc 必須讓它消失。
  - **結局 A (免費): Register Coalescing (暫存器合併):** 如果 `phi` 的來源和結果互不干涉，分配器會把它們分配到**同一個物理暫存器**，`phi` 變成無用指令而被刪除。
  - **结局 B (昂貴): 插入拷貝 (Copies):** 如果暫存器壓力過大，分配器被迫使用**不同**的物理暫存器，此時**必須**在 `phi` 之前的前驅區塊末端，**手動插入 `mv` (拷貝)** 指令來修復邏輯。這些新增的 `mv` 指令會進一步加劇暫存器壓力。

### 步驟 5: Prolog/Epilog Insertion (PEI)

- **運行時機:** **Register Allocation (Step 4) 完成之後**。
- **觸發機制:** 依賴 RegAlloc 交給它的一份「**結案報告**」，報告內容包含：
  - 1. **實際使用了哪些 Callee-Saved 暫存器？**
  - 2. **需要多少 Spill 空間？**
  - 3. **是否需要保存 `ra`？**
- **工作流程:** PEI Pass 像一個「機械化的工人」，根據報告的內容，在函式入口插入 Prologue (打開 stack frame, `sw` 保存 `s0`/`ra` 等)，並在所有 `ret` 指令之前插入 Epilogue (用 `lw` 恢復，關閉 stack frame)。

### 步驟 6 & 7: 晚期優化 & 程式碼發射

- (此為後續課程，我們尚未深入討論)

______________________________________________________________________

## 4. 關鍵工具與除錯流程 (The Debugging Loop)

### A. 工具對比

- **Compiler Explorer (Godbolt):** 理論觀想道場，用於快速實驗。
- **`callgrind` (Valgrind):** 「**模擬器 (Simulator)**」。100% 準確，但極慢，無法反映真實硬體的細微差別。
- **`perf` (Linux Kernel Tool):** 「**硬體採樣器 (Hardware Sampler)**」。直接跑在真實晶片上，耗損極低，是**唯一**能抓到「硬體+軟體」交界處效能問題的工具。

### B. 編譯器工程師的典型除錯循環

1. **症狀 (Symptom):** Profiler (`perf`) 報告顯示，程式有 30% 的時間卡在位址 `0x40051C`。
1. **診斷 (Diagnosis):** 你用 `objdump` 反組譯，發現 `0x40051C` 是一條 `lw s0, 24(sp)` 指令。
1. **假設 (Hypothesis):** 警鈴大作！這是一次昂貴的記憶體讀取，而且是從堆疊載入一個 **Callee-Saved** 暫存器。**這幾乎 100% 是一次 Register Spill！**
1. **調查 (Investigation):** 使用 LLVM 的除錯旗標 (`-print-machineinstrs`, etc.) 重新編譯，追蹤後端流程。
   - **是 RegAlloc 的錯嗎？** 是不是這個函式的「暫存器壓力 (Register Pressure)」真的太高？
   - **是 Pre-RA Scheduler 的錯嗎？** 是不是它排得很爛，無謂地拉長了 Live Ranges？
   - **是 ISel 的錯嗎？** 是不是它選了一組很爛的指令，用了太多不必要的暫存器？
1. **解決 (Solution):** 根據病因，去修改 `.td` 檔裡的 ISel Pattern，或是寫一個 C++ Pass 去優化，從根本上解決問題。

______________________________________________________________________

## Other topics

### What is Phi Instruction and what is Phi Elimination?

#### A. Phi 指令的本質 (The Nature of Phi Instructions)

**什麼是 Phi 指令？**

Phi 指令是 SSA (Static Single Assignment) 形式的核心概念，用於處理**控制流合併點 (Control Flow Join Points)**的問題。

**問題場景：**
```c
int x;
if (condition) {
    x = 10;    // x 在這個路徑被定義為 10
} else {
    x = 20;    // x 在另一個路徑被定義為 20  
}
// 問題：這裡的 x 應該是什麼？
return x * 2;
```

**在 SSA 中的解決方案：**
```llvm
entry:
  br i1 %condition, label %then, label %else

then:
  %x1 = i32 10
  br label %join

else:
  %x2 = i32 20
  br label %join

join:
  %x3 = phi i32 [ %x1, %then ], [ %x2, %else ]  ; 這就是 Phi 指令！
  %result = mul i32 %x3, 2
  ret i32 %result
```

**Phi 指令的語義：**
- `%x3 = phi i32 [ %x1, %then ], [ %x2, %else ]` 的意思是：
  - 如果控制流從 `%then` 基本塊來，則 `%x3 = %x1`
  - 如果控制流從 `%else` 基本塊來，則 `%x3 = %x2`

#### B. 為什麼需要 Phi Elimination？

**核心問題：真實 CPU 沒有 Phi 指令**

Phi 是 SSA 的數學抽象概念，但真實的硬體（如 RISC-V）並沒有 `phi` 這種指令。因此，在 Register Allocation 階段，編譯器必須將 Phi 指令「消滅」，轉換成真實 CPU 能理解的指令。

#### C. Phi Elimination 的兩種結局

##### 結局 A：Register Coalescing (暫存器合併) - 免費的解決方案

**條件：** Phi 的來源變數和結果變數的 Live Range **不重疊**

**策略：** Register Allocator 將它們分配到**同一個物理暫存器**

**範例：**
```llvm
; 原始 LLVM IR
%x3 = phi i32 [ %x1, %then ], [ %x2, %else ]

; 假設 RegAlloc 分配：%x1 -> s1, %x2 -> s1, %x3 -> s1
; 結果：Phi 指令直接消失！無需額外指令。
```

**為什麼可行？** 因為在任何時間點，只有一個變數是「活著的」，所以它們可以安全地共享同一個暫存器。

##### 結局 B：插入拷貝指令 (Copy Insertion) - 昂貴的解決方案

**條件：** 暫存器壓力過高，強迫 RegAlloc 將 Phi 的來源和結果分配到**不同的物理暫存器**

**策略：** 在每個前驅基本塊的末端插入 `mv` (拷貝) 指令

**範例：**
```llvm
; 原始 LLVM IR
%x3 = phi i32 [ %x1, %then ], [ %x2, %else ]

; 假設 RegAlloc 分配：%x1 -> s1, %x2 -> s2, %x3 -> s3
; 編譯器必須插入拷貝指令：

then:
  %x1 = ...           ; 原本的指令
  mv s3, s1          ; 新插入：將 s1 拷貝到 s3
  br join

else:
  %x2 = ...           ; 原本的指令  
  mv s3, s2          ; 新插入：將 s2 拷貝到 s3
  br join

join:
  ; phi 指令消失了，s3 現在包含正確的值
  mul s4, s3, 2      ; %result = %x3 * 2
```

#### D. Phi Elimination 的效能影響

**好消息 (結局 A)：**
- 當 Register Coalescing 成功時，Phi Elimination 是「免費的」
- 沒有額外的指令，沒有效能損失

**壞消息 (結局 B)：**
- 每個 Phi 指令可能變成多條 `mv` 指令
- 增加程式碼大小和執行時間
- **更糟糕的是**：新增的 `mv` 指令會佔用更多暫存器，進一步加劇暫存器壓力，可能引發更多 Spilling

#### E. 對編譯器工程師的啟示

**優化目標：** 盡可能讓 Register Coalescing 成功，避免插入不必要的拷貝指令。

**策略：**
1. **改善 Pre-RA Scheduler：** 更好的指令排程可以縮短 Live Range，減少干涉
2. **Loop Rotation 等優化：** 重新組織控制流，減少 Phi 指令的數量
3. **調整 Register Allocation 演算法：** 在分配決策中考慮 Phi 的 Coalescing 機會