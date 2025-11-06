---
title: RISC-V Learning
author: Yi-Ping Pan (Cloudlet)
---

# RISC-V Learning

## TOC

(我們今天的目標是充分準備 Mediatek compiler engineer 的職缺，有內部消息透露 RISC-V 很重要．所以所有的準備)

- [x] History of RISC-V
- [x] What does it want to solve
- [x] Ecosystem overview
- [ ] RV64GC + RV64V AMP

---

**RV32I - 32-bit 基本整數指令集**

- [ ] 32 個通用暫存器（x0-x31，x0 永遠是 0）
- [ ] 47 條基本指令
- [ ] Load/Store 架構（只有這兩種指令存取記憶體）
- [ ] 簡單的分支和跳躍

**RV64I - 64-bit 基本整數指令集**

- [ ] 擴展到 64-bit 運算
- [ ] 額外的 64-bit 專用指令

**RV128I - 128-bit**

- [ ] 還在開發中

**M Extension - 乘法和除法**

- [ ] MUL, MULH, DIV, REM 等指令
- [ ] 對嵌入式系統很重要

**A Extension - 原子操作（Atomic）**

- [ ] Load-Reserved / Store-Conditional
- [ ] AMO (Atomic Memory Operations)
- [ ] 多核心同步必備

**F Extension - 單精度浮點（32-bit float）**

- [ ] 32 個浮點暫存器（f0-f31）
- [ ] IEEE 754 標準

**D Extension - 雙精度浮點（64-bit double）**

- [ ] 擴展 F Extension

**C Extension - 壓縮指令（16-bit）**

- [ ] 將常用的 32-bit 指令壓縮成 16-bit
- [ ] 節省程式碼大小（重要！）
- [ ] 對嵌入式系統的記憶體很有幫助

**V Extension - 向量運算（Vector）** [KEY!]

- [ ] 為什麼重要：AI workload 的核心
- [ ] 可變長度向量（VLEN 可以是 128, 256, 512...）
- [ ] 與 ARM NEON（固定 128-bit）的差異
- [ ] 與 x86 AVX/AVX-512 的比較
- [ ] 向量暫存器（v0-v31）
- [ ] 向量算術（加減乘除）
- [ ] 向量 Load/Store
- [ ] 向量歸約（reduction）
- [ ] 遮罩操作（masking）
- [ ] 矩陣乘法加速
- [ ] 卷積運算優化
- [ ] 批次處理

**P Extension - Packed-SIMD**

- [ ] 早期提案，已被 V 取代，現在不太用

**B Extension - 位元操作（Bit Manipulation）**

- [ ] 對加密和某些 AI 量化有用

**特權架構**

- [ ] Machine Mode (M-mode) - 最高權限
- [ ] Supervisor Mode (S-mode) - OS kernel
- [ ] User Mode (U-mode) - 應用程式
- [ ] Hypervisor Extension (H) - 虛擬化支援

---

**RISC-V 作為控制處理器**

- [ ] 負責 NPU 的啟動和控制
- [ ] 管理數據搬移（DMA 控制）
- [ ] 處理中斷和異常

**異構運算架構**

- [ ] CPU (RISC-V) + NPU 協同工作
- [ ] 任務分配：輕量運算在 CPU，重運算在 NPU
- [ ] 記憶體共享和一致性

**向量運算的補充**

- [ ] RVV 處理不值得送去 NPU 的小運算
- [ ] 預處理和後處理
- [ ] Activation functions, normalization

**LLVM RISC-V Backend**

- [ ] 如何將 LLVM IR 轉成 RISC-V 指令
- [ ] 指令選擇（Instruction Selection）
- [ ] 寄存器分配（Register Allocation）
- [ ] 指令排程（Instruction Scheduling）

**向量化（Vectorization）**

- [ ] Auto-vectorization：自動將標量程式碼轉向量
- [ ] Loop vectorization
- [ ] SLP vectorization（Superword-Level Parallelism）

**客製化指令生成**

- [ ] 如何利用 RISC-V 的可擴展性
- [ ] 添加 custom instructions for AI
- [ ] Intrinsics 的使用

---

**RISC-V vs ARM**

- [ ] 授權模式：開源 vs 專有
- [ ] 成本：免費 vs 每顆晶片收費
- [ ] 彈性：完全可改 vs 受限
- [ ] 生態成熟度：新興 vs 成熟
- [ ] 性能：相近（看實作）
- [ ] 指令集複雜度：更簡潔 vs 較複雜

**RISC-V vs x86**

- [ ] 複雜度：RISC vs CISC
- [ ] 歷史包袱：乾淨 vs 40 年相容性負擔
- [ ] 功耗：低 vs 高
- [ ] 應用場景：嵌入式/移動 vs 桌面/伺服器

---

**阿里平頭哥 T-Head**

- [ ] 玄鐵系列處理器
- [ ] 用於物聯網和邊緣運算

**為什麼中國積極投入**

- [ ] 美國制裁（ARM 可能被禁）
- [ ] 半導體自主化戰略
- [ ] RISC-V 繞過專利限制

**Andes Technology (晶心科技)**

- [ ] 台灣 RISC-V 處理器 IP 供應商
- [ ] AndesCore 系列

**MediaTek 的策略**

- [ ] 降低對 ARM 的依賴
- [ ] 客製化 AI 加速方案
- [ ] 可能在 IoT/穿戴裝置使用

**其他應用**

- [ ] Western Digital - 每年出貨 10 億顆 RISC-V 核心（硬碟控制器）
- [ ] NVIDIA - GPU 中的微控制器
- [ ] Google - 內部專案
- [ ] SpacemiT - RISC-V 筆電處理器

---

**記憶體模型**

- [ ] Weak Memory Ordering
- [ ] FENCE 指令
- [ ] 與 x86 TSO 的差異

**安全特性**

- [ ] PMP (Physical Memory Protection)
- [ ] TEE (Trusted Execution Environment) 支援
- [ ] 加密擴展提案

**虛擬化**

- [ ] Hypervisor Extension
- [ ] 兩階段地址轉換

---

**開發工具鏈**

- [ ] GNU Toolchain (GCC, Binutils, GDB)
- [ ] LLVM/Clang
- [ ] QEMU - RISC-V 模擬器
- [ ] Spike - 官方 ISA 模擬器

**開源專案**

- [ ] Linux Kernel RISC-V port
- [ ] U-Boot bootloader
- [ ] OpenSBI (Supervisor Binary Interface)

---

**未來趨勢**

- [ ] 伺服器市場進軍（與 ARM Neoverse 競爭）
- [ ] AI 加速器的主流選擇
- [ ] 汽車電子應用
- [ ] 資料中心採用
- [ ] 5G/6G 基礎設施

---

**必須能回答的問題**

- [ ] "什麼是 RISC-V？為什麼重要？"
- [ ] "RISC-V 跟 ARM 有什麼差別？"
- [ ] "RVV (向量擴展) 是什麼？為什麼對 AI 重要？"
- [ ] "RISC-V 在 NPU 系統中扮演什麼角色？"
- [ ] "你會如何將 AI 算子編譯到 RISC-V？"
- [ ] "為什麼 MediaTek 要投資 RISC-V？"

**展現你的理解**

- [ ] 連結到 NPU Compiler 工作
- [ ] 提到 LLVM backend
- [ ] 討論向量化優化
- [ ] 理解異構運算架構
- [ ] 知道產業趨勢（中美科技戰、開源生態）

**學習資源（之後深入用）**

- [ ] 官方網站：riscv.org
- [ ] 《Computer Organization and Design RISC-V Edition》
- [ ] 《The RISC-V Reader》
- [ ] RISC-V 規格文件（riscv.org/specifications）
- [ ] LLVM RISC-V 文檔
- [ ] Andes 技術文件（台灣公司！）

---

## The basics of RISC-V

### History of RISC-V

RISC-V 誕生於 2010 年，由 UC Berkeley 的 Krste Asanović 教授團隊創建。這個名字中的 "V" 代表第五代 RISC 架構—前四代都是 Berkeley 的研究專案，但都沒有對外開放。

關鍵的里程碑：

- **2010**: 專案啟動
- **2015**: 成立 RISC-V International（非營利組織），推動標準化
- **2020**: 總部從美國搬到瑞士，避免美國出口管制的影響

這個搬遷決定非常關鍵—它確保了 RISC-V 能夠真正成為「全球開放標準」，不受地緣政治限制。

### What Problems Does RISC-V Solve?

RISC-V 的出現解決了半導體產業的幾個根本性問題：

**1. 授權費用問題**

- ARM 對每顆晶片收取授權費（royalty fee）
- 對於出貨量大的產品（如 IoT 設備），累積費用驚人
- RISC-V 完全開源，零授權費

**2. 供應商鎖定 (Vendor Lock-in)**

- 一旦選擇 ARM 或 x86，整個生態系統都被綁死
- 無法自由遷移到其他架構
- RISC-V 讓你掌握自己的命運

**3. 學術研究的法律障礙**

- 過去研究 ARM/x86 需要簽 NDA，甚至無法發表論文
- RISC-V 完全公開，學術界可以自由研究和創新

**4. 架構的戰略獨立性**

- 特別是在中美科技戰背景下
- ARM 可能被禁運（華為就是例子）
- RISC-V 提供了一條戰略安全的路徑

**5. 歷史包袱問題**

- x86 有 40+ 年的相容性負擔，指令集臃腫
- 為了向後相容，保留了大量過時的設計
- RISC-V 從零開始，沒有歷史包袱

**6. 客製化的困難**

- ARM 不允許你隨意修改指令集
- 想加自己的加速指令？不行
- RISC-V 完全開放，想怎麼改就怎麼改

### Why RISC-V is Designed This Way (從 NPU Compiler 視角)

理解 RISC-V 的設計哲學對 compiler 工程師至關重要。這些設計決策直接影響我們如何生成高效的機器碼。

#### 1. 模組化設計哲學 (Modular ISA)

RISC-V 採用 **Base ISA + Optional Extensions** 的架構。這不是簡單的功能切割，而是深思熟慮的設計。

**為什麼這對 NPU 系統重要？**

- 控制核心可能只需要 `RV32IMC`（32-bit 整數 + 乘除法 + 壓縮指令）
- AI 運算核心需要 `RV64IMAFDCV`（64-bit + 原子操作 + 單/雙精度浮點 + 壓縮 + 向量）
- 這樣的配置可以節省大量晶片面積和功耗

**Compiler 的角度：**

```c
// Compiler 可以針對不同配置生成最佳化程式碼
#ifdef __riscv_vector
  // 使用向量指令
  vsetvl_e32m1(vl);
  vadd_vv(...);
#else
  // 回退到標量版本
  for (int i = 0; i < n; i++)
    c[i] = a[i] + b[i];
#endif
```

#### 2. 固定長度指令 (32-bit) + 可選壓縮指令 (16-bit)

**為什麼選擇 32-bit 固定長度？**

- **解碼簡單**：每個指令都是 4-byte 對齊，不需要複雜的長度解碼器
- **Pipeline 乾淨**：Fetch/Decode/Execute 邊界清晰
- **對比 x86**：x86 指令長度可變（1-15 bytes），解碼器佔晶片面積的 20%+

**C Extension 的巧妙之處：**

```assembly
# 原始 32-bit 指令
addi x10, x10, 1    # 4 bytes

# C Extension 壓縮版
c.addi x10, 1       # 2 bytes (節省 50%)
```

實測數據：啟用 C Extension 可以減少 25-30% 的程式碼大小，對嵌入式系統的記憶體預算至關重要。

**Compiler Backend 的挑戰：**
LLVM RISC-V backend 需要智能選擇何時使用壓縮指令：

- 常用的立即數範圍 -> 用壓縮指令
- 需要完整 32-bit 立即數 -> 用標準指令
- 這是 instruction selection 階段的優化

#### 3. Load-Store 架構的深意

**核心原則：只有 Load/Store 可以存取記憶體，所有運算都在暫存器進行。**

```assembly
# RISC-V 方式
lw   x10, 0(x11)    # Load from memory
add  x10, x10, x12  # Register operation
sw   x10, 0(x13)    # Store to memory

# x86 方式 (可以直接對記憶體操作)
add  [mem], reg     # Memory operand
```

**為什麼這樣設計？**

- **簡化 Pipeline**：記憶體存取和運算分離，不會互相阻塞
- **提高頻率**：硬體設計更簡單，可以跑更高的時脈
- **對 NPU Compiler 的好處**：記憶體存取模式清晰，容易做 DMA 規劃和 prefetching

**實務案例：矩陣乘法**

```c
// Compiler 看到這段程式碼
for (int i = 0; i < N; i++)
  for (int j = 0; j < N; j++)
    C[i][j] += A[i][k] * B[k][j];

// 可以清楚地識別：
// 1. 哪些是 load (A, B)
// 2. 哪些是 compute (乘法、加法)
// 3. 哪些是 store (C)
// -> 更容易做 software pipelining 和 DMA 規劃
```

#### 4. 大量通用暫存器 (32 個)

**為什麼是 32 個而不是 16 個？**

更多暫存器 = 更少的 spilling（當暫存器不夠時，被迫把資料存回記憶體）

**Register Count Comparison Across Architectures:**

| Architecture        | Integer Registers               | Float/SIMD Registers             | Design Era | Notes                     |
| ------------------- | ------------------------------- | -------------------------------- | ---------- | ------------------------- |
| **x86 (32-bit)**    | 8 (eax-edx, esi, edi, ebp, esp) | 8 (xmm0-7)                       | 1978       | Heavy legacy burden       |
| **x86-64**          | 16 (rax-r15)                    | 16 (xmm0-15) / 32 (AVX-512)      | 2003       | Compatibility compromise  |
| **ARM (32-bit)**    | 16 (r0-r15)                     | 32 (s0-s31 or d0-d15)            | 1985       | Classic RISC config       |
| **ARM64 (AArch64)** | 31 + SP                         | 32 (v0-v31, 128-bit)             | 2011       | Major improvement         |
| **MIPS**            | 32 ($0-$31)                     | 32 (f0-f31)                      | 1985       | RISC pioneer              |
| **ARC**             | 32 or 64 (configurable)         | 32                               | 1996       | Highly customizable       |
| **RISC-V**          | 32 (x0-x31)                     | 32 (f0-f31) + 32 vector (v0-v31) | 2010       | Modern clean-slate design |

**In-depth Analysis:**

**x86's Historical Lesson:**

```assembly
# x86 32-bit nightmare
# Only 8 registers, and most have special purposes:
# eax: Accumulator (many instructions hardwired to it)
# ecx: Counter (used by loop instructions)
# edx: Data register (used by mul/div)
# ebx: Base register
# esp: Stack pointer (can't use freely)
# ebp: Base pointer (frame pointer)
# esi, edi: Index registers (string operations)

# Only ~6 registers available for general use!
# Compiler constantly needs to spill to memory
mov [esp-4], eax    # Spill
# ... do other work
mov eax, [esp-4]    # Reload
```

**x86-64's Improvement (But Still Insufficient):**

```assembly
# Extended to 16, but still not enough for modern AI workloads
# Loop unrolling is limited:
movss xmm0, [rsi]       # a[i]
movss xmm1, [rsi+4]     # a[i+1]
movss xmm2, [rsi+8]     # a[i+2]
movss xmm3, [rsi+12]    # a[i+3]
# ... can only unroll 4x before register pressure
# If you need to preserve other variables, immediately need to spill
```

**ARM64 vs RISC-V: Almost Identical Choices**

```assembly
# ARM64 (AArch64)
# 31 general-purpose registers (x0-x30) + zero register (xzr) + SP
# 32 vector registers (v0-v31, 128-bit)

# RISC-V
# 32 general-purpose registers (x0-x31, x0 hardwired to zero)
# 32 floating-point registers (f0-f31)
# 32 vector registers (v0-v31, variable length)

# Both learned from x86's mistakes!
```

**MIPS's Foresight:**

- Adopted 32 registers back in 1985
- Proved the RISC philosophy: use more registers to reduce memory traffic
- RISC-V continues this legacy (the "V" partly honors MIPS)

**Why 32 is the Sweet Spot:**

1. **Encoding Space**: 5 bits encodes 32 registers (2^5 = 32)

   ```
   ┌────────┬─────┬─────┬─────┬────────┬────────┐
   │ opcode │ rd  │ rs1 │ rs2 │ funct3 │ funct7 │
   └────────┴─────┴─────┴─────┴────────┴────────┘
              5bit  5bit  5bit
   ```

2. **Silicon Area**: 32 registers have reasonable hardware cost

   - 64 would need 6 bits (longer instructions)
   - 128 would be too expensive

3. **Compiler Research**: Empirical evidence shows 32 is sufficient for most cases
   - 16 is often insufficient (x86-64's experience)
   - 64 shows diminishing returns

**Actual Spilling Rate Comparison (from academic research):**

**Spilling rate** 是暫存器不夠用時，被迫把資料存回記憶體的頻率。Memory access 比 register 慢 10-100 倍，所以這是效能殺手。LLVM Backend 的 Register Allocation Pass 使用 graph coloring 演算法來決定何時 spill。RISC-V 有 32 個暫存器，spilling rate 只有 2%，遠低於 x86 的 15%。更多暫存器 = 更少 spilling = 更高效能。

```text
Architecture    Register Count    Avg Spill Rate (SPECint)
──────────────────────────────────────────────────────────
x86 (32-bit)         8                    ~15%
x86-64              16                     ~8%
ARM64               31                     ~3%
RISC-V              32                     ~2%
```

**Compiler 寄存器分配器的福音：**

- 32 個暫存器讓 graph coloring 更容易找到合法的分配
- 減少 spill code，提高性能

#### 5. 沒有條件執行和複雜定址模式

**ARM 的條件執行範例：**

```arm
CMP  r0, r1
ADDEQ r2, r3, r4  # 只在相等時執行加法
```

**RISC-V 的選擇：**

```riscv
bne  x10, x11, skip
add  x12, x13, x14
skip:
```

**為什麼放棄條件執行？**

- **硬體簡單**：不需要在每個指令裡塞條件欄位
- **Pipeline 效率**：現代 CPU 有 branch predictor，條件執行的優勢已經不大
- **Compiler 自由**：讓 compiler 決定要用 branch 還是 conditional move (`select` in LLVM IR)

**定址模式的取捨：**

```c
// ARM 可以：LDR r0, [r1, r2, LSL #2]  (base + index << 2)
// RISC-V：需要兩條指令
slli x3, x2, 2
add  x3, x1, x3
lw   x10, 0(x3)
```

看起來變多了？但硬體更簡單，而且現代 CPU 的 micro-op fusion 可以優化這種模式。

#### 6. 可擴展性：Custom Instructions 的威力

RISC-V 保留了 **custom opcode 空間**，讓你加自己的指令！

**MediaTek NPU 的實際應用場景：**

```assembly
# 假設 MediaTek 加了一個量化指令
custom.quant.int8 v1, v2, scale, zero_point
# ^ 一條指令完成量化！

# 而不是用一堆標準指令模擬（5-6 條）
vmul.vf v1, v2, scale      # 1. 除以 scale
vadd.vi v1, v1, zero_point # 2. 加 zero_point
vmax.vi v1, v1, -128       # 3. Clipping 下界
vmin.vi v1, v1, 127        # 4. Clipping 上界
```

**量化 (Quantization) 快速理解：**

| Concept              | Description                                               | Example                                  |
| -------------------- | --------------------------------------------------------- | ---------------------------------------- |
| **Purpose**          | Convert FP32 (4 bytes) to INT8 (1 byte), save 4x memory   | ResNet-50: 102MB -> 25MB                 |
| **Formula**          | `INT8 = clip(round(FP32/scale) + zero_point, -128, 127)`  | FP32=1.5 -> INT8=50                      |
| **scale**            | Scaling factor = (max-min)/255, controls precision        | Data range -1~1 -> scale=0.00784         |
| **zero_point**       | Zero-point alignment, which INT8 value maps to FP32's 0.0 | Symmetric quant=0, Asymmetric=-128       |
| **Performance Gain** | INT8 ops 2-4x faster than FP32 (hardware dependent)       | Tensor Core: INT8 is 4x faster than FP32 |
| **Accuracy Loss**    | Usually <2% accuracy drop (after fine-tuning)             | ImageNet Top-1: 76.1% -> 75.8%           |
| **Why Custom Inst**  | Standard instructions need 5-6, custom only needs 1       | 5-6x speedup + power saving              |

**LLVM 整合方式：**

1. **Intrinsics**:

```c
__builtin_riscv_custom_quant_int8(data, scale, zero_point);
```

2. **Inline Assembly**:

```c
asm volatile ("custom.quant.int8 %0, %1, %2, %3"
              : "=v"(out) : "v"(in), "f"(scale), "i"(zp));
```

3. **Pattern Matching**:
   Compiler 可以自動識別量化的 pattern，替換成 custom instruction。

#### 7. 向量擴展 (RVV) 的革命性設計 [CRITICAL]

這是 RISC-V 最厲害的地方，也是為什麼它對 AI 如此重要。本節將深入探討 Vector Length Agnostic (VLA) 設計、Tail Handling 機制，以及它們與 MLIR Fallback 和 LLVM Backend 的關聯。

---

### 7.1 核心概念：Vector Length Agnostic (VLA)

**VLA 的革命性承諾：Write Once, Run Anywhere**

```c
void vector_add(float *a, float *b, float *c, size_t n) {
  size_t vl;  // Vector Length - 硬體會告訴你這次能處理多少元素

  for (size_t i = 0; i < n; i += vl) {
    // Step 1: 詢問硬體「你能處理多少？」
    vl = vsetvl_e32m1(n - i);
    // vsetvl = "vector set vector length"
    // e32 = element width 32-bit (float)
    // m1 = LMUL=1 (使用 1 個向量暫存器)
    // 硬體回傳：min(硬體能力, 剩餘元素數)

    // Step 2-4: 向量運算（語法與標量類似）
    vfloat32m1_t va = vle32_v_f32m1(&a[i], vl);  // Vector Load
    vfloat32m1_t vb = vle32_v_f32m1(&b[i], vl);  // Vector Load
    vfloat32m1_t vc = vfadd_vv_f32m1(va, vb, vl); // Vector Add
    vse32_v_f32m1(&c[i], vc, vl);                 // Vector Store
  }
  // 重點：單一 loop，無需 cleanup code！
}
```

**同一份 binary 在不同硬體的執行行為：**

| Hardware    | VLEN    | vl (max) | Elements/Iter | Iterations (n=100) |
| ----------- | ------- | -------- | ------------- | ------------------ |
| IoT Chip    | 128-bit | 4        | 4 floats      | 25                 |
| Mobile Chip | 256-bit | 8        | 8 floats      | 13 (last vl=4)     |
| Server Chip | 512-bit | 16       | 16 floats     | 7 (last vl=4)      |

**核心優勢：**

- 同一份二進制檔案自動適應硬體能力
- 硬體升級 -> 自動加速（無需重新編譯）
- 自動處理任意長度（n=33, 97 皆可）

---

### 7.2 Tail Handling：RISC-V 如何消除 Cleanup Loop

**問題定義：尾端元素處理**

在向量化程式設計中，**Tail Handling（尾端處理）**是指處理陣列長度不是向量寬度整數倍時，剩餘元素的處理方式。這是所有 SIMD 架構都必須面對的問題，而 RISC-V 的解決方案是最優雅的。

**範例：n=33, 向量寬度=4**

- 33 ÷ 4 = 8 次完整向量操作 + **剩餘 1 個元素**
- 這 1 個元素怎麼處理？這是傳統架構的痛點

#### 傳統架構：手動 Cleanup Loop 的代價

```c
// [BAD] ARM NEON 的傳統做法
void neon_add(float *a, float *b, float *c, int n) {
    int i;

    // 第一階段：向量化處理（只能處理 4 的倍數）
    for (i = 0; i + 3 < n; i += 4) {
        float32x4_t va = vld1q_f32(&a[i]);  // Load 4 個 float
        float32x4_t vb = vld1q_f32(&b[i]);
        float32x4_t vc = vaddq_f32(va, vb);
        vst1q_f32(&c[i], vc);
    }

    // 第二階段：Cleanup Loop（標量處理剩餘元素）
    // [BAD] 這是額外的程式碼！
    // [BAD] 這會引入額外的分支！
    // [BAD] Pipeline 效率降低！
    for (; i < n; i++) {
        c[i] = a[i] + b[i];  // 標量運算，慢得多
    }
}

// n=33 的執行情況：
// Loop 1: i=0,  處理 a[0..3]   [OK] 向量化
// Loop 1: i=4,  處理 a[4..7]   [OK] 向量化
// Loop 1: i=8,  處理 a[8..11]  [OK] 向量化
// Loop 1: i=12, 處理 a[12..15] [OK] 向量化
// Loop 1: i=16, 處理 a[16..19] [OK] 向量化
// Loop 1: i=20, 處理 a[20..23] [OK] 向量化
// Loop 1: i=24, 處理 a[24..27] [OK] 向量化
// Loop 1: i=28, 處理 a[28..31] [OK] 向量化
// Loop 2: i=32, 處理 a[32]     [BAD] 退化成標量！
```

**為什麼 Cleanup Loop 是個問題？**

1. **程式碼複雜度增加**：

   - 需要寫兩份邏輯（向量版 + 標量版）
   - 容易出 bug（兩個 loop 的邊界條件）

2. **分支預測失敗**：

   - CPU 的 branch predictor 需要預測 cleanup loop 會不會執行
   - 如果 n 變化頻繁（如神經網路的不同層），預測失敗率高

3. **效能損失**：

   ```
   向量加法延遲：    1 cycle (4 個 float)
   標量加法延遲：    1 cycle (1 個 float)

   如果尾端有 3 個元素：
   - 向量版能在 1 cycle 處理（如果硬體支援部分 masking）
   - 標量版需要 3 cycles
   ```

4. **Compiler 優化困難**：
   - Auto-vectorizer 需要生成兩種程式碼路徑
   - Loop unrolling 受限（展開後尾端處理更複雜）

**RISC-V 的革命性解決方案：Hardware-Managed Tail**

RISC-V 透過 `vsetvl` 指令和 **向量遮罩（Vector Masking）** 機制，讓硬體自動處理尾端：

```c
// [GOOD] RISC-V 的優雅方式
void riscv_add(float *a, float *b, float *c, size_t n) {
  size_t vl;  // Vector Length - 這次能處理多少元素

  for (size_t i = 0; i < n; i += vl) {
    // 關鍵！硬體會自動調整 vl
    vl = vsetvl_e32m1(n - i);

    // 硬體保證：vl = min(VLMAX, n-i)
    // - VLMAX = 硬體最大向量長度
    // - n-i = 剩餘元素數

    // 後續的向量指令自動只處理 vl 個元素
    vfloat32m1_t va = vle32_v_f32m1(&a[i], vl);
    vfloat32m1_t vb = vle32_v_f32m1(&b[i], vl);
    vfloat32m1_t vc = vfadd_vv_f32m1(va, vb, vl);
    vse32_v_f32m1(&c[i], vc, vl);
    // 注意：只有一個 loop！沒有 cleanup code！
  }
}

// n=33 的執行情況（假設 VLMAX=4）：
// i=0:  vl=4, 處理 a[0..3]
// i=4:  vl=4, 處理 a[4..7]
// i=8:  vl=4, 處理 a[8..11]
// i=12: vl=4, 處理 a[12..15]
// i=16: vl=4, 處理 a[16..19]
// i=20: vl=4, 處理 a[20..23]
// i=24: vl=4, 處理 a[24..27]
// i=28: vl=4, 處理 a[28..31]
// i=32: vl=1, 處理 a[32]      [GOOD] 自動調整！仍然用向量指令！
```

**核心機制：vsetvl 的運作原理**

```assembly
# vsetvl rd, rs1, vtypei
# rd  = 輸出的 vl（向量長度）
# rs1 = 請求的元素數量（Application Vector Length, AVL）
# vtypei = 向量類型配置（element width, LMUL, etc.）

vsetvli t0, a0, e32, m1, ta, ma
# t0 <-- 硬體回傳的 vl
# a0 = 剩餘元素數（n-i）
# e32 = element width 32-bit
# m1 = LMUL=1（使用 1 個暫存器）
# ta = tail agnostic（尾端元素的值不重要）
# ma = mask agnostic（遮罩外的元素值不重要）

# 硬體的計算邏輯：
# vl = min(VLMAX, a0)
# 其中 VLMAX = (VLEN / 32) * LMUL
#            = (128 / 32) * 1 = 4  (假設 VLEN=128)
```

**向量遮罩的硬體實作**

當 `vl < VLMAX` 時，硬體自動生成一個內部的 mask：

```text
假設 VLMAX=4，但這次 vl=1（只剩 1 個元素）

內部 mask = [1, 0, 0, 0]
            ↑  ↑  ↑  ↑
            處 忽 忽 忽
            理 略 略 略

vadd.vv v1, v2, v3, vl
# 硬體實際執行：
# v1[0] = v2[0] + v3[0]  <- 有效
# v1[1] = <不改變>        <- 被 mask
# v1[2] = <不改變>        <- 被 mask
# v1[3] = <不改變>        <- 被 mask

# 關鍵：這仍然是一條向量指令！
# 硬體透過內部的 mask 實現部分執行
```

**Tail Agnostic vs Tail Undisturbed**

RISC-V 提供兩種尾端處理策略：

```c
// Tail Agnostic (ta)
vsetvli t0, a0, e32, m1, ta, ma
vadd.vv v1, v2, v3
// v1 中 vl 之外的元素可以是任意值（可能被硬體覆寫）
// 優勢：硬體可以優化，不需要保留舊值

// Tail Undisturbed (tu)
vsetvli t0, a0, e32, m1, tu, ma
vadd.vv v1, v2, v3
// v1 中 vl 之外的元素保持原有的值
// 用於需要保留暫存器部分內容的場景
```

**實戰案例：矩陣轉置的 Tail Handling**

```c
// 轉置一個 N×M 的矩陣（N 和 M 可能不是向量長度的倍數）
void transpose(float *src, float *dst, int N, int M) {
  for (int i = 0; i < N; i++) {
    size_t vl;
    for (int j = 0; j < M; j += vl) {
      // 關鍵：每一行的尾端長度可能不同
      vl = vsetvl_e32m1(M - j);

      // Strided load（跳躍存取）
      vfloat32m1_t v = vlse32_v_f32m1(&src[i*M + j], M*sizeof(float), vl);

      // 連續 store
      vse32_v_f32m1(&dst[j*N + i], v, vl);
    }
  }
  // 無論 M 是多少，都不需要額外的 cleanup code！
}
```

**與其他架構的對比總結**

| Feature              | ARM NEON             | x86 AVX-512                  | RISC-V RVV               |
| -------------------- | -------------------- | ---------------------------- | ------------------------ |
| **Vector Length**    | 固定 128-bit         | 固定 512-bit                 | 可變 (VLA)               |
| **Tail Handling**    | 手動 cleanup loop    | 手動 cleanup / k-register    | 硬體自動 (vsetvl)        |
| **Code Complexity**  | 高（兩個 loop）      | 中-高（需要 mask register）  | 低（單一 loop）          |
| **Branch Count**     | +1 (cleanup 判斷)    | +1 (cleanup 判斷)            | 0 (無額外分支)           |
| **Portability**      | 固定寬度，無法擴展   | 固定寬度，較舊 CPU 無法執行  | 同一 binary 適應所有硬體 |
| **Hardware Support** | 廣泛（手機、嵌入式） | 有限（高階伺服器、降頻問題） | 新興（但生態快速成長）   |

---

### 7.3 豐富的 Load/Store Patterns

RISC-V RVV 提供多種記憶體存取模式，直接對應 AI workload 的需求：

```assembly
# 1. Unit-stride：連續記憶體存取（最常見）
vle32.v v1, (a0)         # Load 連續的 vl 個 float
vse32.v v1, (a0)         # Store 連續的 vl 個 float

# 2. Strided：跳躍存取（用於 layout 轉換）
vlse32.v v1, (a0), a1    # Load，每次跳 a1 bytes
                          # 應用：NCHW → NHWC 轉換

# 3. Indexed (Scatter/Gather)：稀疏存取
vlxei32.v v1, (a0), v2   # Load，index 來自 v2
                          # 應用：稀疏矩陣、embedding lookup

# 4. Segment：交錯結構存取
vlseg3e32.v v1, v2, v3, (a0)  # Load 3 個交錯的向量
                              # 應用：RGB 像素、AoS ↔ SoA
```

這些 pattern 直接對應 AI workload 的常見需求，compiler 可以直接映射，不需要複雜的 shuffle。

---

### 7.4 與 MLIR Fallback 和 LLVM Backend 的關聯

**NPU Compiler 的完整 Lowering Pipeline：**

```text
High-Level (Python/TensorFlow/PyTorch)
          |
          v
    MLIR (linalg dialect)
          |
          v
    MLIR (vector dialect) <-- Vectorization 決策點
          |
          v
    MLIR (LLVM dialect)
          |
          v
    LLVM IR (target-independent)
          |
          v
    LLVM IR (RISC-V intrinsics) <-- RVV 特化
          |
          v
    RISC-V Assembly (SelectionDAG)
          |
          v
    Machine Code
```

#### 7.4.1 MLIR Fallback 機制

**什麼時候 Fallback？**

在 NPU compiler 中，並非所有操作都在 NPU 上執行。某些情況下需要 fallback 到 CPU（RISC-V control core）：

```mlir
// 原始 MLIR（linalg dialect）
func.func @model(%input: tensor<1x224x224x3xf32>)
    -> tensor<1x1000xf32> {

  // 1. Conv2D - 在 NPU 執行（custom lowering）
  %conv1 = linalg.conv_2d_nhwc_hwcf
    ins(%input, %weights1: ...)

  // 2. BatchNorm - 可能 fallback 到 RISC-V
  //    因為 NPU 可能沒有專用硬體
  %bn1 = linalg.generic {
    // ...batch norm computation...
  }

  // 3. ReLU - 在 NPU 執行（簡單操作）
  %relu1 = linalg.generic {
    // max(0, x)
  }

  return %result : tensor<1x1000xf32>
}
```

**Fallback Decision（Compiler 內部邏輯）：**

```python
# 在 MLIR Pass 中
def should_fallback_to_cpu(op):
    if op.is_conv2d():
        return False  # NPU 擅長
    elif op.is_matmul():
        if op.output_size < 1024:
            return True  # 太小，不值得送 NPU
        return False
    elif op.is_batchnorm():
        if target.has_bn_engine():
            return False  # NPU 有專用引擎
        return True  # Fallback to RISC-V
    elif op.is_custom():
        return True  # 未知操作，CPU 處理
```

**Fallback 後的 RVV 向量化：**

```mlir
// BatchNorm fallback 到 RISC-V
func.func @batchnorm_riscv(%input: tensor<?xf32>,
                           %mean: f32, %var: f32)
    -> tensor<?xf32> {

  // Lower 到 vector dialect
  %c0 = arith.constant 0 : index
  %c1 = arith.constant 1 : index
  %len = tensor.dim %input, %c0 : tensor<?xf32>

  %result = scf.for %i = %c0 to %len step %vl {
    // vsetvl 動態設定向量長度
    %vl = riscv.vsetvl %remaining, e32, m1

    // 向量化的 (x - mean) / sqrt(var + eps)
    %v_in = vector.load %input[%i] : vector<?xf32>
    %v_sub = arith.subf %v_in, %mean : vector<?xf32>
    %v_norm = arith.divf %v_sub, %std : vector<?xf32>
    vector.store %v_norm, %result[%i] : vector<?xf32>
  }

  return %result : tensor<?xf32>
}
```

**關鍵：VLA 讓 Fallback 也能高效**

- 即使 fallback 到 CPU，仍能利用 RVV 加速
- 無需手動處理尾端（不同 batch size 自動適應）
- 同一份 compiler 生成的程式碼可在不同 RISC-V 核心執行

#### 7.4.2 LLVM Backend 的 RVV Codegen

**LLVM IR 中的 VLA 表示：**

```llvm
; LLVM IR 使用 <vscale x n x type> 表示 VLA 向量
define void @vector_add(float* %a, float* %b, float* %c, i64 %n) {
entry:
  %i = alloca i64
  store i64 0, i64* %i
  br label %loop

loop:
  %idx = load i64, i64* %i
  %remaining = sub i64 %n, %idx

  ; vsetvl intrinsic - 設定向量長度
  %vl = call i64 @llvm.riscv.vsetvl.i64(i64 %remaining,
                                         i64 0,      ; AVL
                                         i64 3)      ; vtype (e32, m1)

  ; 向量 load（vscale 表示長度可變）
  %pa = getelementptr float, float* %a, i64 %idx
  %pb = getelementptr float, float* %b, i64 %idx
  %pc = getelementptr float, float* %c, i64 %idx

  %va = call <vscale x 4 x float> @llvm.riscv.vle.nxv4f32(
    <vscale x 4 x float> undef, float* %pa, i64 %vl)
  %vb = call <vscale x 4 x float> @llvm.riscv.vle.nxv4f32(
    <vscale x 4 x float> undef, float* %pb, i64 %vl)

  ; 向量加法
  %vc = fadd <vscale x 4 x float> %va, %vb

  ; 向量 store
  call void @llvm.riscv.vse.nxv4f32(<vscale x 4 x float> %vc,
                                     float* %pc, i64 %vl)

  ; 更新 loop index
  %next = add i64 %idx, %vl
  store i64 %next, i64* %i
  %cond = icmp ult i64 %next, %n
  br i1 %cond, label %loop, label %exit

exit:
  ret void
}

; 內建函數聲明（由 LLVM RISC-V backend 提供）
declare i64 @llvm.riscv.vsetvl.i64(i64, i64, i64)
declare <vscale x 4 x float> @llvm.riscv.vle.nxv4f32(
  <vscale x 4 x float>, float*, i64)
declare void @llvm.riscv.vse.nxv4f32(
  <vscale x 4 x float>, float*, i64)
```

**關鍵設計：`<vscale x n x type>`**

- `vscale`：一個 runtime constant，表示 VLEN 的倍數
- 編譯時不知道具體長度，但知道長度的"比例"
- 讓 LLVM 的優化 pass 仍能運作（如 CSE, DCE, GVN）

**LLVM Backend 的 Instruction Selection：**

```cpp
// llvm/lib/Target/RISCV/RISCVISelLowering.cpp
SDValue RISCVTargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) {
  switch (Op.getOpcode()) {
  case ISD::INTRINSIC_WO_CHAIN: {
    unsigned IntNo = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();

    // vsetvl intrinsic
    if (IntNo == Intrinsic::riscv_vsetvl) {
      SDValue AVL = Op.getOperand(1);
      SDValue VType = Op.getOperand(3);

      // 生成 RISC-V vsetvl 指令
      return DAG.getNode(RISCVISD::VSETVL, DL, VT, AVL, VType);
    }

    // vle (vector load)
    if (IntNo == Intrinsic::riscv_vle) {
      SDValue Ptr = Op.getOperand(2);
      SDValue VL = Op.getOperand(3);

      // 生成 vle32.v 指令
      return DAG.getNode(RISCVISD::VLE, DL, VT, Ptr, VL);
    }
    // ... 其他 intrinsics
  }
  }
}
```

**Register Allocation 的挑戰：**

```text
RISC-V 有 32 個向量暫存器 (v0-v31)
但 LMUL > 1 時會消耗多個暫存器！

LMUL=1: 使用 1 個暫存器（如 v1）
LMUL=2: 使用 2 個暫存器（如 v2, v3 - 必須連續）
LMUL=4: 使用 4 個暫存器（如 v4, v5, v6, v7）
LMUL=8: 使用 8 個暫存器（如 v8-v15）

Compiler 需要智能選擇 LMUL：
- LMUL 大 → 一次處理更多元素 → 減少 loop overhead
- LMUL 大 → 暫存器壓力增加 → 可能 spilling
```

**Auto-Vectorization：Loop Vectorizer**

```cpp
// LLVM Loop Vectorizer 看到這段程式碼
for (int i = 0; i < n; i++)
  c[i] = a[i] + b[i];

// 決策流程：
// 1. 檢查是否有 dependency（無）
// 2. 檢查 target 是否支援向量化（RISC-V RVV: YES）
// 3. Cost model：向量化是否划算？
//    - Loop trip count 預估
//    - 向量化 overhead（vsetvl）
//    - Memory bandwidth
// 4. 如果通過，生成 VLA 向量化程式碼

// 生成的 LLVM IR（簡化版）
for (i = 0; i < n; i += vl) {
  vl = vsetvl(n - i);
  va = vle(&a[i], vl);
  vb = vle(&b[i], vl);
  vc = vadd(va, vb);
  vse(&c[i], vc, vl);
}
// 注意：無 cleanup loop！
```

---

### 7.5 架構對比與 Compiler 視角的總結

| Feature                 | ARM NEON               | x86 AVX-512            | RISC-V RVV                        |
| ----------------------- | ---------------------- | ---------------------- | --------------------------------- |
| **Vector Length**       | 固定 128-bit           | 固定 512-bit           | 可變 (VLA)                        |
| **Tail Handling**       | 手動 cleanup loop      | k-register masking     | 硬體自動 (vsetvl)                 |
| **LLVM Codegen**        | 固定寬度 vector type   | 固定寬度 vector type   | `<vscale x n>`                    |
| **Binary Portability**  | 無（固定寬度）         | 無（固定寬度）         | [YES]（同一 binary 適應所有硬體） |
| **Auto-Vec Success**    | 中等（尾端處理複雜）   | 中等（mask 複雜）      | 高（無尾端問題）                  |
| **Fallback Efficiency** | 低（NEON 不夠靈活）    | 低（AVX 不靈活）       | 高（VLA 適應小 workload）         |
| **MLIR Integration**    | 需要多個 lowering path | 需要多個 lowering path | 單一 VLA path                     |

**為什麼 VLA 對 NPU Compiler 是遊戲規則改變者？**

1. **簡化 Fallback 路徑**

   - 小運算 fallback 到 CPU 仍能高效（RVV 自動適應）
   - 不需要為每個 tensor shape 生成專門程式碼

2. **統一的 MLIR Lowering**

   - 不需要 "if VLEN=128 then ... else if VLEN=256 then ..."
   - 單一 vector dialect lowering path

3. **減少 Compiler Binary Size**

   - 不需要為不同 VLEN 編譯多個版本
   - 簡化 deployment

4. **提高 Portability**
   - 今天編譯 → 未來硬體自動加速
   - 降低維護成本

**面試完美答案範例：**

> "RISC-V RVV 的 Vector Length Agnostic 設計透過 vsetvl 指令讓硬體動態告知向量長度，配合內部 masking 機制自動處理尾端，完全消除了 cleanup loop。
>
> 在 NPU compiler 中，這有兩大優勢：
>
> 1. **Fallback 路徑高效**：小運算 fallback 到 RISC-V control core 時，仍能用 RVV 加速，且自動適應不同 batch size。
> 2. **簡化 MLIR lowering**：從 linalg dialect 到 LLVM IR 的 lowering 可以用單一 VLA path，不需要為不同硬體生成多個版本。
>
> LLVM backend 用 `<vscale x n x type>` 表示 VLA 向量，讓 auto-vectorizer 可以積極優化而無需擔心尾端處理，大幅提升向量化成功率。"

#### 8. 記憶體一致性模型 (Weak Memory Model)

RISC-V 採用比 x86 TSO 更弱的記憶體模型。

**什麼意思？**

- 硬體可以重排序記憶體存取（在符合 dependency 的前提下）
- 提高記憶體並行度，特別是多核心系統
- 但需要程式設計師/compiler 顯式使用 `FENCE` 指令來同步

**CPU-NPU 協同的挑戰：**

```c
// CPU 端
tensor_data[0] = 42;
npu_mailbox[0] = TASK_READY;  // 通知 NPU

// 如果沒有 fence，NPU 可能先看到 mailbox，但 tensor_data 還沒寫入！
```

**Compiler 需要插入 fence：**

```c
tensor_data[0] = 42;
__sync_synchronize();  // Compiler 會生成 fence 指令
npu_mailbox[0] = TASK_READY;
```

**LLVM IR 層面：**

```llvm
store i32 42, i32* %tensor_data
fence seq_cst  ; 確保順序
store i32 1, i32* %npu_mailbox
```

#### 9. 原子操作 (A Extension) 的雙重設計

RISC-V 提供兩種原子操作：

**1. LR/SC (Load-Reserved/Store-Conditional)** - Optimistic Concurrency

```assembly
// C++ code:
//   std::atomic<int> counter = 100;
//   counter.fetch_add(1);
// Result: counter = 101

// Compiled to RISC-V:
retry:
  lr.w    a0, (a1)         # Load word + establish reservation
                           # a0 = mem[a1] = 100
                           # Hardware marks this address for monitoring

  addi    a0, a0, 1        # Modify in register (any complex operation allowed)
                           # a0 = 101

  sc.w    a2, a0, (a1)     # Store-Conditional: check if anyone else wrote
                           # If SUCCESS: mem[a1] = 101, a2 = 0
                           # If FAIL:    mem[a1] unchanged, a2 = 1

  bnez    a2, retry        # If a2 != 0 (failed), retry from beginning
```

**Key Concepts:**

- **Instruction suffix**: `.w` = Word (32-bit), `.d` = Double (64-bit)
- **Memory ordering**: `.aq` = acquire, `.rl` = release, `.aqrl` = seq_cst
- **Atomicity is HARDWARE-guaranteed** (via cache coherency protocol like MESI)
- **Compiler only generates instructions** (recognizes semantics + inserts fences)

**Hardware Reservation Mechanism:**

```
lr.w execution:
  1. Read value from memory
  2. Record reservation_address in CPU core's reservation set
  3. Monitor other CPUs' writes to this address via cache coherency

sc.w execution:
  1. Check if reservation is still valid
  2. If VALID and no other CPU wrote to this address:
     → Write succeeds, return 0 in destination register
  3. If INVALID (someone else wrote):
     → Write fails, return 1 in destination register

Reservation invalidated by:
  - Other CPU writes to monitored address
  - Context switch (OS thread switch)
  - Timeout (implementation-dependent)
```

**2. AMO (Atomic Memory Operations)** - Single Instruction

```assembly
// C++ code:
//   std::atomic<int> counter = 1;
//   int old = counter.fetch_add(5);
// Result: old = 1, counter = 6

// Compiled to RISC-V (single instruction!):
li      x12, 5               # x12 = 5 (value to add)
la      x11, counter         # x11 = &counter
amoadd.w x10, x12, (x11)     # Atomic operation

// What amoadd.w does (atomically):
//   Step 1: x10 = mem[x11]           → x10 = 1  (read old value)
//   Step 2: temp = mem[x11] + x12    → temp = 1 + 5 = 6
//   Step 3: mem[x11] = temp          → counter = 6
// Final state: x10 = 1 (old), counter = 6 (new)

// Other AMO operations:
amoswap.w  x10, x12, (x11)   # x10 = mem[x11]; mem[x11] = x12;
amoand.w   x10, x12, (x11)   # x10 = mem[x11]; mem[x11] &= x12;
amoor.w    x10, x12, (x11)   # x10 = mem[x11]; mem[x11] |= x12;
amoxor.w   x10, x12, (x11)   # x10 = mem[x11]; mem[x11] ^= x12;
amomin.w   x10, x12, (x11)   # x10 = mem[x11]; mem[x11] = min(mem[x11], x12);
amomax.w   x10, x12, (x11)   # x10 = mem[x11]; mem[x11] = max(mem[x11], x12);
```

**LR/SC vs. AMO Comparison:**

| Feature      | LR/SC                                      | AMO                                       |
| ------------ | ------------------------------------------ | ----------------------------------------- |
| Instructions | 3-4 (lr + ops + sc + branch)               | 1 (single instruction)                    |
| Operations   | Any complex logic (CAS, conditional, etc.) | Fixed 8 ops (add/swap/and/or/xor/min/max) |
| Efficiency   | Fast when no contention, retry overhead    | Always fast, no retry                     |
| Use case     | Complex synchronization primitives         | Simple counters, flags                    |

**Real-world Example: NPU Job Queue (Multi-producer, Multi-consumer)**

```c
// Producer thread: enqueue job to NPU
void enqueue(job_t *job) {
  int tail;
  do {
    // Atomically load current tail index
    tail = __atomic_load_n(&queue->tail, __ATOMIC_ACQUIRE);
    // Try to atomically increment tail (CAS operation)
  } while (!__atomic_compare_exchange_n(&queue->tail, &tail, tail+1,
                                        false, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE));
  // If we got here, we own slot 'tail'
  queue->jobs[tail] = job;  // Compiler generates LR/SC for CAS
}

// Alternative using AMO (simpler but less control):
void enqueue_simple(job_t *job) {
  // Atomically fetch-and-increment tail
  int slot = __atomic_fetch_add(&queue->tail, 1, __ATOMIC_RELAXED);
  // Compiler generates: amoadd.w for fetch_add
  queue->jobs[slot % QUEUE_SIZE] = job;
}
```

#### 10. 沒有浮點運算的 Condition Codes

**x86 的做法：**

```x86
ucomisd xmm0, xmm1  ; 比較結果寫入 EFLAGS
jb      label        ; 根據 flags 跳躍
```

**RISC-V 的做法：**

```riscv
flt.d x10, f0, f1   ; 比較結果直接寫入整數暫存器 x10
bnez  x10, label    ; 根據整數暫存器跳躍
```

**為什麼這樣更好？**

- **解耦**：浮點單元不需要處理 flags，簡化設計
- **低延遲**：不需要等待 flags 更新
- **Compiler 友善**：比較結果是明確的值，更容易做 CSE (Common Subexpression Elimination)

#### 11. 特權模式分層 (M/S/U)

```
M-mode (Machine)     → 最高權限，bootloader, firmware
   ↓
S-mode (Supervisor)  → OS kernel, device drivers
   ↓
U-mode (User)        → User applications
```

**NPU 系統的權限分配：**

- **M-mode**: NPU firmware 初始化
- **S-mode**: NPU device driver，記憶體管理
- **U-mode**: AI runtime (TensorFlow Lite, ONNX Runtime)

**安全性：PMP (Physical Memory Protection)**

```c
// Kernel 可以限制 NPU 只能存取特定記憶體區域
pmp_set_region(0, npu_buffer_start, npu_buffer_end, PMP_R | PMP_W);
```

防止 NPU 越權存取系統記憶體，提高安全性。

### 為什麼這些設計對 NPU Compiler 至關重要

讓我們整合一下，從 end-to-end 的視角看：

**1. 簡單清晰的指令集 → 更可預測的性能模型**

```python
# 在 MLIR 做 cost modeling
def estimate_latency(op):
  if op.type == "vector_add":
    # RISC-V 的向量加法延遲是確定的
    return VLEN / 32 * 4  # 4 cycles per vector add
  # x86 的 AVX-512 在某些 CPU 會降頻...
```

**2. 模組化 → 異構系統的最佳配置**

```
┌──────────────┐
│ NPU Cluster  │
├──────────────┤
│ Compute Core │ <-- RV64IMAFDCV (全功能)
│ Compute Core │
├──────────────┤
│ Control Core │ <-- RV32IMC (精簡)
└──────────────┘
```

**3. 可擴展性 → 針對 AI workload 定製**

```c
// MediaTek 可以加這些 custom instructions
custom.matmul.int8   // 8-bit 矩陣乘法
custom.softmax       // Softmax 融合指令
custom.layernorm     // Layer normalization

// Compiler pattern matching
if (detect_matmul_pattern())
  emit_custom_matmul_inst();
```

**4. 開源 → Compiler 開發者完全掌控**

- 可以看到完整的 ISA spec
- 可以提交 patch 到 LLVM upstream
- 不像 ARM，某些指令的細節是保密的

### 對比：為什麼不用 x86/ARM 做 NPU 控制核心？

| Feature                | RISC-V     | ARM              | x86          |
| ---------------------- | ---------- | ---------------- | ------------ |
| **Power Consumption**  | Ultra-low  | Low              | High         |
| **Complexity**         | Simple     | Medium           | Very Complex |
| **Licensing Fee**      | Free       | Per-chip royalty | Expensive    |
| **Customization**      | Fully open | Restricted       | Impossible   |
| **Legacy Burden**      | None       | Some             | 40+ years    |
| **Ecosystem Maturity** | Growing    | Mature           | Mature       |

**結論：**

- **x86**: 太複雜、功耗高、無法客製化 → 不適合
- **ARM**: 授權費、不能隨意修改、某些場景被政治因素影響 → 有風險
- **RISC-V**: 乾淨、開源、可客製化、生態正在快速成熟 → 最佳選擇

---

## RISC-V Ecosystem Overview (從 NPU Compiler 視角)

理解 RISC-V 生態系統對於 NPU compiler 工程師至關重要，因為你需要知道：

- 哪些硬體平台可以測試你的 compiler
- 哪些工具鏈可以整合
- 哪些公司在推動相關技術（潛在合作或競爭對手）

### 硬體供應商與處理器 IP

#### **SiFive - 美國的 RISC-V 先鋒**

- **地位**：最早的商業化 RISC-V 公司（2015 年成立）
- **產品線**：
  - **Performance 系列**（P550, P670）：高性能應用處理器
  - **Intelligence 系列**（X280）：AI/ML 專用，帶向量擴展
  - **Essential 系列**（E24, E34）：嵌入式微控制器
- **對 NPU Compiler 的意義**：
  - X280 有完整的 RVV 1.0 實作，是測試向量化 compiler 的理想平台
  - SiFive 與 Intel 合作，推動 RISC-V 進入資料中心

```c
// SiFive X280 的向量特性
// VLEN = 512-bit (可配置到 1024-bit)
// ELEN = 64-bit
// LMUL = 8 (最大可用 8 個向量暫存器合併)

// 這意味著 compiler 可以生成高度並行的程式碼
vsetvli t0, a0, e32, m8  // 使用 m8 (8個暫存器合併)
vle32.v v0, (a1)          // 一次 load 512-bit * 8 = 4KB!
```

#### **Andes Technology (晶心科技) - 台灣之光** [IMPORTANT]

- **地位**：全球第三大 CPU IP 供應商（僅次於 ARM、Synopsys）
- **為什麼重要**：
  - **台灣本土公司**，MediaTek 可能採用（地緣優勢）
  - 在嵌入式市場深耕 20+ 年
  - 支援完整的 RISC-V 擴展（包括 V extension）
- **產品線**：
  - **AndesCore 25 系列**：32-bit 嵌入式
  - **AndesCore 45 系列**：64-bit 應用處理器
  - **AX 系列**：帶向量擴展的 AI 處理器
- **技術特色**：
  - **StackSafe**：硬體堆疊保護（防止 buffer overflow）
  - **CoDense**：程式碼壓縮技術（比標準 C Extension 更強）
  - **PowerBrake**：動態功耗管理

**實際案例：Andes 在 NPU 系統中的角色**

```
┌─────────────────────────────────────┐
│         MediaTek NPU SoC            │
├─────────────────────────────────────┤
│  Andes AX45MP (Control Core)        │
│  - RV64IMAFDCV                      │
│  - NPU initialization               │
│  - DMA and interrupt handling       │
├─────────────────────────────────────┤
│  Proprietary NPU Cores              │
│  - Matrix computation units         │
│  - Dedicated quantization engine    │
└─────────────────────────────────────┘
```

#### **Alibaba T-Head (平頭哥) - 中國的野心**

- **背景**：阿里巴巴全資子公司（2018 年成立）
- **戰略意義**：中國半導體自主化的關鍵
- **玄鐵系列處理器**：
  - **玄鐵 910**：64-bit 高性能核心（12-stage pipeline）
  - **玄鐵 907**：中端應用處理器
  - **玄鐵 902**：嵌入式 MCU
- **技術亮點**：
  - 自研的向量擴展實作（早於 RVV 1.0 標準）
  - 開源了 xuantie-gnu-toolchain（GCC 移植）
  - 與 Android 整合（T-Head 的晶片可以跑 Android）

**玄鐵 910 的效能數據**：

```
SPECint2006: 7.5/GHz (與 ARM Cortex-A73 相當)
功耗: 0.3 mW/MHz (非常省電)
目標應用: 5G 基站、智慧家居、邊緣運算
```

**對 Compiler 工程師的挑戰**：

- T-Head 有自己的 **RVV 0.7.1** 實作（與標準 RVV 1.0 不同）
- Compiler 需要支援多個 RVV 版本（conditional compilation）

```c
#if defined(__riscv_v_intrinsic) && (__riscv_v_intrinsic >= 1000000)
  // RVV 1.0 標準
  vsetvl_e32m1(vl);
#elif defined(__riscv_v_intrinsic) && (__riscv_v_intrinsic >= 7000)
  // T-Head RVV 0.7
  vsetvli(vl, avl, e32, m1);
#endif
```

#### **StarFive - RISC-V 的開發板之王**

- **產品**：VisionFive 系列單板電腦
- **意義**：讓開發者可以在真實硬體上測試 RISC-V Linux
- **VisionFive 2 規格**：
  - CPU: JH7110 (4x SiFive U74 @ 1.5GHz)
  - GPU: IMG BXE-4-32 (支援 OpenGL ES 3.2)
  - RAM: 2GB/4GB/8GB LPDDR4
  - 可以跑完整的 Debian/Ubuntu
- **對 Compiler 開發的價值**：
  - 可以實際測試 auto-vectorization 的效果
  - Benchmark AI workload 的性能
  - 除錯 memory ordering 相關的 bug

---

### 軟體工具鏈（Compiler 工程師的武器庫）

#### **GCC (GNU Compiler Collection)**

- **RISC-V 支援狀態**：
  - RV32I/RV64I: [FULL] 完整支援（GCC 7.1+）
  - RVV 1.0: [FULL] 完整支援（GCC 14+）
  - Auto-vectorization: [BASIC] 基本支援，但不如 LLVM 成熟

**GCC 的 RISC-V 向量化範例**：

```c
// 原始程式碼
void vector_add(int *a, int *b, int *c, int n) {
  for (int i = 0; i < n; i++)
    c[i] = a[i] + b[i];
}

// GCC 使用 -march=rv64gcv -O3 -ftree-vectorize 後生成：
vector_add:
  vsetvli a3, a3, e32, m1, ta, ma
  vle32.v v1, (a0)
  vle32.v v2, (a1)
  vadd.vv v1, v1, v2
  vse32.v v1, (a2)
  // ... loop handling
```

**GCC vs LLVM 的取捨**：

| Feature                   | GCC                | LLVM        |
| ------------------------- | ------------------ | ----------- |
| **Vectorization Quality** | Basic              | Excellent   |
| **Compilation Speed**     | Fast               | Slow        |
| **Debug Info**            | Traditional stable | More modern |
| **Customization Ease**    | Difficult          | Easy        |
| **Industry Adoption**     | Embedded focus     | AI/ML focus |

#### **LLVM/Clang - NPU Compiler 的首選** [***CRITICAL***]

**為什麼 LLVM 對 NPU Compiler 如此重要？**

1. **MLIR (Multi-Level IR)**：
   - 可以表達高階的 AI 操作（linalg, tensor, etc.）
   - 逐步 lower 到 RISC-V 機器碼
   - 這是 Google TPU Compiler 和 TVM 的基礎

```mlir
// MLIR 中的矩陣乘法
func.func @matmul(%A: tensor<1024x1024xf32>,
                  %B: tensor<1024x1024xf32>) -> tensor<1024x1024xf32> {
  %C = linalg.matmul ins(%A, %B : tensor<1024x1024xf32>, tensor<1024x1024xf32>)
                     outs(%C : tensor<1024x1024xf32>) -> tensor<1024x1024xf32>
  return %C : tensor<1024x1024xf32>
}

// Lower 到 RISC-V Vector
// 1. Tiling
// 2. Vectorization (使用 RVV)
// 3. Register allocation
// 4. Instruction scheduling
```

2. **Polyhedral 優化**：

   - Loop interchange, tiling, fusion
   - 對 AI workload 的效能影響巨大

3. **向量化引擎**：
   - Loop Vectorizer：自動向量化迴圈
   - SLP Vectorizer：Superword-Level Parallelism
   - 針對 RISC-V RVV 有專門優化

**LLVM RISC-V Backend 的關鍵檔案**（面試會問！）：

```
llvm/lib/Target/RISCV/
├── RISCVInstrInfo.td          # 指令定義
├── RISCVInstrInfoV.td         # 向量指令定義
├── RISCVISelLowering.cpp      # Instruction selection
├── RISCVRegisterInfo.td       # 暫存器定義
├── RISCVTargetMachine.cpp     # Target configuration
└── RISCVVectorPeephole.cpp    # 向量優化 pass
```

**如何貢獻到 LLVM RISC-V Backend**：

```bash
# 1. Clone LLVM
git clone https://github.com/llvm/llvm-project.git

# 2. 建立 feature branch
git checkout -b riscv-custom-opt

# 3. 修改 (例如：加入 custom instruction support)
vim llvm/lib/Target/RISCV/RISCVInstrInfo.td

# 4. 建置測試
ninja check-llvm-codegen-riscv

# 5. 提交 patch 到 Phabricator (LLVM 的 code review 平台)
```

---

#### **Linux Kernel RISC-V Port**

- **支援狀態**：Mainline since Linux 4.15 (2018)
- **關鍵維護者**：Palmer Dabbelt (Google), Guo Ren (Alibaba)
- **對 NPU Driver 開發的影響**：
  - Memory management (頁表、DMA)
  - 中斷處理 (IRQ handling)
  - Device tree binding

**NPU Driver 的典型架構**：

```c
// drivers/misc/mediatek_npu.c
static int mtk_npu_probe(struct platform_device *pdev) {
  struct mtk_npu_device *npu;

  // 1. 記憶體映射
  npu->reg_base = devm_ioremap_resource(&pdev->dev, res);

  // 2. 中斷註冊
  npu->irq = platform_get_irq(pdev, 0);
  devm_request_irq(&pdev->dev, npu->irq, mtk_npu_irq_handler, ...);

  // 3. DMA 設定
  dma_set_mask_and_coherent(&pdev->dev, DMA_BIT_MASK(36));

  // 4. Character device 註冊
  cdev_init(&npu->cdev, &mtk_npu_fops);
}

// 使用者空間可以透過 ioctl 呼叫 NPU
// ioctl(fd, MTK_NPU_RUN_MODEL, &model_desc);
```

### 作業系統支援

#### **Linux - 主流選擇**

- **發行版支援**：
  - Debian: [OFFICIAL] 官方支援 (riscv64 port)
  - Ubuntu: [OFFICIAL] 22.04 LTS+ 支援
  - Fedora: [FULL] 完整支援
  - Arch Linux: [COMMUNITY] 社群支援
- **應用場景**：高性能運算、伺服器、AI workload

#### **FreeRTOS - 嵌入式首選**

- **特性**：Real-time OS，極低記憶體佔用 (< 10KB)
- **RISC-V 支援**：官方 port，支援多種 RISC-V 核心
- **應用場景**：NPU 的 firmware、IoT 邊緣設備

**FreeRTOS 在 NPU 中的角色**：

```c
// NPU 內部的 RISC-V 控制核心跑 FreeRTOS
void npu_task_scheduler(void *params) {
  while (1) {
    // 1. 從 command queue 取任務
    if (xQueueReceive(npu_cmd_queue, &cmd, portMAX_DELAY)) {
      // 2. 配置 NPU 硬體
      npu_configure(cmd.model_id);

      // 3. 啟動 DMA
      dma_start(cmd.input_addr, cmd.input_size);

      // 4. 等待完成中斷
      ulTaskNotifyTake(pdTRUE, portMAX_DELAY);

      // 5. 回報結果
      xQueueSend(npu_result_queue, &result, 0);
    }
  }
}
```

#### **Zephyr - 現代 RTOS**

- **特色**：模組化、安全性強（支援 ARM TrustZone 概念）
- **RISC-V 支援**：多板支援（HiFive1, ESP32-C3, etc.）
- **優勢**：Devicetree、Kconfig（與 Linux 類似的配置系統）

---

### 開發板與評估套件

#### **硬體對比表**：

| Board                | CPU                   | RAM       | Price   | Use Case                     |
| -------------------- | --------------------- | --------- | ------- | ---------------------------- |
| **HiFive Unmatched** | SiFive U740 (5 cores) | 16GB      | $665    | High-performance development |
| **VisionFive 2**     | JH7110 (4 cores)      | 2-8GB     | $60-100 | Linux development            |
| **Milk-V Mars**      | JH7110                | 2-8GB     | $50     | Entry-level learning         |
| **Sipeed Lichee RV** | Allwinner D1          | 512MB-1GB | $20     | IoT prototyping              |

**如何選擇開發板**（針對 NPU Compiler 開發）：

1. **需要測試向量化**？→ 選有 RVV support 的（如 VisionFive 2）
2. **需要跑完整 Linux**？→ 至少 2GB RAM
3. **只是學習 ISA**？→ QEMU 模擬器即可（免費！）

---

### 產業支持與戰略意義

#### **Google - 雲端巨頭的投資**

- **動機**：
  - 降低對 ARM 的依賴
  - Android 生態的多元化
  - TPU 內部可能採用 RISC-V 微控制器
- **貢獻**：
  - Android RISC-V port (正在進行)
  - LLVM 的資金支持
  - 雲端基礎設施的實驗

#### **NVIDIA - GPU 廠商的微妙角色**

- **實際應用**：GPU 內部的微控制器使用 RISC-V（取代 Falcon）
- **為什麼不是核心運算**？GPU shader core 仍是專有架構
- **對 AI compiler 的啟示**：異構系統中，RISC-V 適合做控制，不一定是運算主力

#### **Qualcomm - 手機晶片的對沖策略**

- **動機**：與 ARM 的授權糾紛（Nuvia 收購案）
- **投資**：加入 RISC-V International 成為戰略成員
- **可能方向**：未來的 Snapdragon 晶片可能包含 RISC-V 核心

#### **Samsung - 記憶體與 AI 晶片**

- **應用**：SSD 控制器、IoT 晶片
- **AI 野心**：Samsung Exynos NPU 可能採用 RISC-V 控制核心

#### **Intel - x86 巨頭的轉向**

- **戰略**：收購 SiFive 的傳聞（未成）
- **Intel Foundry Services**：提供 RISC-V 晶片代工
- **動機**：x86 市場萎縮，需要新的成長點

---

### 中國的 RISC-V 投資狂潮

**為什麼中國如此積極？**

1. **美國制裁的陰影**：

   - 華為被禁用 ARM（2019）
   - 中興通訊被制裁（2018）
   - RISC-V 是唯一的「安全」選擇

2. **半導體自主化**：

   - 中國製造 2025 計畫
   - 目標：2025 年晶片自給率 70%
   - RISC-V 是繞過專利的捷徑

3. **投資規模**：
   - 平頭哥（阿里）：數十億人民幣
   - 中科院（香山處理器）：國家級專案
   - 地方政府補助：深圳、上海等地大力支持

**中國 RISC-V 聯盟（CRVIC）**：

- 成員：華為、阿里、騰訊、百度、小米...
- 目標：統一 RISC-V 標準（避免碎片化）
- 對全球的影響：可能形成「中國標準」vs「國際標準」

---

#### **印度的 Shakti 處理器專案**

- **背景**：IIT Madras（印度理工學院）主導
- **目標**：印度本土的處理器 IP
- **進展**：已成功流片（22nm）
- **意義**：發展中國家也能參與晶片設計（RISC-V 的民主化）

---

### NPU Compiler 工程師應該關注什麼？

1. **硬體趨勢**：

   - RVV 1.0 的普及（編譯器需要跟上）
   - 客製化指令的爆發（需要可擴展的 compiler 架構）
   - 異構系統成為常態（RISC-V + NPU/GPU）

2. **工具鏈成熟度**：

   - LLVM > GCC（AI/ML workload）
   - MLIR 是未來（多層級 IR）
   - 需要掌握 Polyhedral 優化

3. **生態系統的碎片化風險**：

   - 不同廠商的 RVV 版本
   - 客製化指令的不相容
   - Compiler 需要支援多個 variant

4. **產業機會**：
   - MediaTek（台灣）、SiFive（美國）、平頭哥（中國）都在招 compiler 工程師
   - 薪資水平：台灣 150-250 萬/年，美國 $150k-250k/年
   - 未來 5 年是 RISC-V 的黃金時期

---

### 實戰：如何快速上手 RISC-V 開發

```bash
# 1. 安裝 RISC-V 工具鏈
sudo apt install gcc-riscv64-unknown-elf

# 2. 使用 QEMU 模擬器
sudo apt install qemu-system-riscv64

# 3. 編譯一個簡單的程式
riscv64-unknown-elf-gcc -march=rv64gcv -o hello hello.c

# 4. 在 QEMU 中執行
qemu-riscv64 ./hello

# 5. 使用 LLVM 查看生成的向量指令
clang -target riscv64 -march=rv64gcv -O3 -S -emit-llvm hello.c -o hello.ll
llc -march=riscv64 -mattr=+v hello.ll -o hello.s
```

**面試時的加分項**：

- 能說出晶心科技的 AndesCore 系列
- 了解 T-Head 的玄鐵處理器
- 知道 VisionFive 2 開發板
- 理解為什麼 LLVM 比 GCC 更適合 AI compiler
- 能討論中國 RISC-V 投資的地緣政治因素

---

## RV64GC + RV64V AMP

### RISC-V 關鍵術語解析：NPU Compiler 面試必備

#### RV64GC - 通用處理器的標準配置

**RV64GC 是什麼？**

RV64GC 是 RISC-V 最常見的配置縮寫，代表：

```text
RV64GC = RV64IMAFD_Zicsr_Zifencei + C

拆解：
RV64   = 64-bit 基礎架構
G      = "General" 的縮寫，包含 IMAFD
  I    = Integer (基本整數指令集)
  M    = Multiply/Divide (乘除法)
  A    = Atomic (原子操作)
  F    = Float (單精度浮點)
  D    = Double (雙精度浮點)
C      = Compressed (壓縮指令，16-bit)
```

**為什麼叫 "G"？**

歷史上，IMAFD 這個組合太常用了，RISC-V 社群就簡稱為 "G" (General-purpose)，方便表示通用處理器的標準配置。

**完整展開：**

RV64GC = RV64I + M + A + F + D + Zicsr + Zifencei + C

其中：

- Zicsr: CSR (Control and Status Register) 指令
- Zifencei: 指令記憶體屏障（用於 self-modifying code）

**在 NPU 系統中的角色：**

```text
┌─────────────────────────────────────────┐
│         MediaTek NPU SoC                │
├─────────────────────────────────────────┤
│  Control Processor (RV64GC)             │ <-- Run Linux, system management
│  - Operating system responsibility      │
│  - Device driver                        │
│  - Task scheduling                      │
│  - DMA configuration                    │
├─────────────────────────────────────────┤
│  Compute Cores (RV64GCV)                │ <-- Handle AI computation
│  - Vector operations (V extension)      │
│  - Fallback path                        │
│  - Data preprocessing/postprocessing    │
├─────────────────────────────────────────┤
│  NPU Hardware Accelerator               │ <-- Dedicated hardware
│  - Matrix engines                       │
│  - Quantization units                   │
└─────────────────────────────────────────┘
```

**Compiler 視角：**

```c
// 編譯時指定 target
clang -target riscv64 -march=rv64gc -O3 program.c

// Compiler 知道可以使用：
// - 64-bit 整數運算
// - 硬體乘除法 (不需要軟體模擬)
// - 原子操作 (用於 lock-free 資料結構)
// - 浮點運算 (FP32 + FP64)
// - 壓縮指令 (節省 25-30% code size)
```

**面試重點：**

**Q: "為什麼 NPU 的 control core 通常用 RV64GC 而不是更簡單的 RV64I？"**

**A**: "因為需要：

1. **M extension**: device driver 中常有乘除法運算
2. **A extension**: 多核心同步（spinlock, atomic counters）
3. **F/D extension**: 某些 control logic 需要浮點運算（如 DMA bandwidth 計算）
4. **C extension**: 減少記憶體佔用，對嵌入式系統很重要"

---

#### RV64V - AI/ML 的專用武器

**RV64V 是什麼？**

RV64V = RV64I + V extension

V = Vector Extension (向量擴展)

這是 RISC-V 為了 AI/ML workload 專門設計的指令集擴展，支援 **Vector Length Agnostic (VLA)** 設計。

**完整配置通常是：**

RV64GCV = RV64GC + V

也就是：

- 有 GC 的所有功能（通用處理）
- 加上 V extension（向量加速）

**V Extension 的核心能力：**

```assembly
# 1. 動態向量長度設定
vsetvli t0, a0, e32, m1, ta, ma
# t0 = 硬體回傳的 vl
# a0 = 請求處理的元素數

# 2. 向量記憶體操作
vle32.v v1, (a0)        # Vector load
vse32.v v1, (a0)        # Vector store
vlse32.v v1, (a0), a1   # Strided load (用於 layout 轉換)

# 3. 向量算術
vadd.vv v1, v2, v3      # v1 = v2 + v3 (element-wise)
vmul.vv v1, v2, v3      # v1 = v2 * v3
vfmacc.vf v1, f0, v2    # v1 += f0 * v2 (FMA: fused multiply-add)

# 4. 向量歸約 (Reduction)
vredsum.vs v1, v2, v3   # v1 = sum(v2) + v3
```

**與其他架構的對比：**

| Feature             | ARM NEON            | x86 AVX2            | x86 AVX-512        | RISC-V RVV             |
| ------------------- | ------------------- | ------------------- | ------------------ | ---------------------- |
| **Vector Width**    | Fixed 128-bit       | Fixed 256-bit       | Fixed 512-bit      | Variable (VLA)         |
| **Max Elements**    | 4 float32           | 8 float32           | 16 float32         | VLEN/32 (variable)     |
| **Tail Handling**   | Manual cleanup loop | Manual cleanup loop | k-register masking | Hardware auto (vsetvl) |
| **Portability**     | None                | None                | None               | Complete               |
| **AI Friendliness** | Medium              | Medium              | High               | Excellent              |

**在 NPU Compiler 中的使用場景：**

```python
# MLIR lowering 決策樹
def lower_operation(op):
    if op.is_large_matmul():
        return lower_to_npu(op)  # 送到 NPU 硬體

    elif op.is_small_matmul():
        # 小矩陣不值得送 NPU (overhead 太高)
        return lower_to_rvv(op)  # Fallback 到 RV64V

    elif op.is_elementwise():
        # Element-wise 操作（如 ReLU, Add）
        if op.size > threshold:
            return lower_to_npu(op)
        else:
            return lower_to_rvv(op)  # RVV 更靈活

    elif op.is_custom():
        return lower_to_rvv(op)  # 未知操作，CPU 處理
```

**實際 Compiler 生成的程式碼：**

```c
// 原始 C code (AI 的 element-wise add)
void add_bias(float *data, float *bias, int n) {
    for (int i = 0; i < n; i++)
        data[i] += bias[i];
}

// LLVM auto-vectorization 生成的 RVV code
void add_bias_rvv(float *data, float *bias, int n) {
    size_t vl;
    for (size_t i = 0; i < n; i += vl) {
        vl = vsetvl_e32m1(n - i);
        vfloat32m1_t vd = vle32_v_f32m1(&data[i], vl);
        vfloat32m1_t vb = vle32_v_f32m1(&bias[i], vl);
        vfloat32m1_t vr = vfadd_vv_f32m1(vd, vb, vl);
        vse32_v_f32m1(&data[i], vr, vl);
    }
    // 注意：沒有 cleanup loop！
}
```

**面試重點：**

**Q: "RVV 對 NPU compiler 最大的價值是什麼？"**

**A**: "三點：

1. **高效的 Fallback 路徑**：小運算不值得送 NPU 時，RVV 提供高效的 CPU fallback
2. **統一的 Code Generation**：VLA 設計讓 compiler 不需要為不同硬體生成多個版本
3. **簡化 Auto-Vectorization**：無需處理 tail cleanup，提高向量化成功率"

---

#### AMP - Asymmetric Multi-Processing（非對稱多核心處理）

**AMP 是什麼？**

AMP 是一種 **異構多核心架構**，不同的核心有不同的配置和功能，各司其職。

**對比：SMP vs AMP**

```text
SMP (Symmetric Multi-Processing)
┌────────┐ ┌────────┐ ┌────────┐ ┌────────┐
│ Core 0 │ │ Core 1 │ │ Core 2 │ │ Core 3 │
│ RV64GC │ │ RV64GC │ │ RV64GC │ │ RV64GC │ <-- 所有核心相同
└────────┘ └────────┘ └────────┘ └────────┘
     │          │          │          │
     └──────────┴──────────┴──────────┘
              共享 L2 Cache

優點：負載平衡容易，程式設計簡單
缺點：無法針對不同任務優化


AMP (Asymmetric Multi-Processing)
┌─────────────┐  ┌────────────┐  ┌───────────┐
│   Big Core  │  │  Mid Core  │  │ Tiny Core │
│   RV64GCV   │  │   RV64GC   │  │  RV64IMC  │ <-- 核心配置不同
│  2.5 GHz    │  │   1.5 GHz  │  │  800 MHz  │
│ Out-of-Order│  │  In-order  │  │  Simple   │
└─────────────┘  └────────────┘  └───────────┘
       │               │              │
       │               │              │
  AI workload    General tasks    Background
  RVV 向量運算    Device driver     DMA engine

優點：功耗效率高，針對性能優化
缺點：負載平衡複雜，需要智能調度
```

**MediaTek NPU 的 AMP 架構實例：**

```text
MediaTek Dimensity 9300 (hypothetical)
┌─────────────────────────────────────────┐
│         Application Processor           │
├─────────────────────────────────────────┤
│  "Big" Cluster                          │
│  ├─ 4x Cortex-X4 @ 3.25 GHz             │ <-- High-performance CPU
│  │  (ARM, not RISC-V)                   │
├─────────────────────────────────────────┤
│  "Little" Cluster                       │
│  ├─ 4x Cortex-A720 @ 2.0 GHz            │ <-- Power-efficient CPU
├─────────────────────────────────────────┤
│  NPU Subsystem (RISC-V AMP)             │
│  ├─ Control Core: RV64GC @ 1.0 GHz      │ <-- Management core
│  │   • Runs FreeRTOS                    │
│  │   • Device driver                    │
│  │   • Task queue management            │
│  ├─ Compute Core 0: RV64GCV @ 1.5 GHz   │ <-- AI computation
│  │   • With RVV extension               │
│  │   • Fallback path                    │
│  ├─ Compute Core 1: RV64GCV @ 1.5 GHz   │
│  ├─ DMA Engine: RV32IMC @ 600 MHz       │ <-- Data movement
│  │   • Low-power design                 │
│  │   • Pure DMA control                 │
│  └─ NPU Hardware (Non-programmable)     │ <-- Fixed-function hardware
│      • Matrix engines (INT8/INT16)      │
│      • Quantization units               │
└─────────────────────────────────────────┘
```

**為什麼 NPU 用 AMP？**

**1. 功耗優化**

Control Core (RV64GC):

- 功耗：~50 mW
- 任務：輕量級控制
- 大部分時間在等待

Compute Core (RV64GCV):

- 功耗：~200 mW (active), ~10 mW (idle)
- 任務：向量運算
- 只在需要時啟動

DMA Engine (RV32IMC):

- 功耗：~20 mW
- 任務：記憶體搬移
- 簡單設計，極低功耗

**2. 成本優化**

如果全部用 RV64GCV：

- 晶片面積大 (向量單元佔空間)
- 成本高
- 但 control core 根本用不到向量運算！

AMP 設計：

- Control core 用簡單的 RV64GC (小面積)
- 只有需要的 core 用 RV64GCV
- 成本降低 30-40%

**3. 性能優化**

不同核心專注不同任務：

- Control core: 低延遲響應中斷
- Compute core: 高吞吐量向量運算
- DMA engine: 持續的記憶體搬移

**Compiler 對 AMP 的支援：**

```c
// Compiler 需要生成針對不同 core 的程式碼

// 1. Control Core (RV64GC)
void npu_driver_init(void) {
    // 編譯參數：-march=rv64gc -O2
    // 不能使用 vector 指令！
    configure_dma();
    setup_interrupts();
    init_task_queue();
}

// 2. Compute Core (RV64GCV)
void vector_workload(float *data, int n) {
    // 編譯參數：-march=rv64gcv -O3 -ftree-vectorize
    // 可以使用 RVV 指令
    size_t vl;
    for (size_t i = 0; i < n; i += vl) {
        vl = vsetvl_e32m1(n - i);
        // ... vector operations
    }
}

// 3. DMA Engine (RV32IMC)
void dma_copy_task(void *src, void *dst, size_t len) {
    // 編譯參數：-march=rv32imc -Os (optimize for size)
    // 極簡程式碼，功耗優先
    dma_start(src, dst, len);
    while (!dma_done());
}
```

**LLVM 的 Multi-target Build：**

```bash
# Makefile for AMP system
# 需要為不同 core 編譯不同版本

# Control Core
clang -target riscv64 -march=rv64gc -O2 \
      -c control.c -o control_rv64gc.o

# Compute Core
clang -target riscv64 -march=rv64gcv -O3 -ftree-vectorize \
      -c compute.c -o compute_rv64gcv.o

# DMA Engine
clang -target riscv32 -march=rv32imc -Os \
      -c dma.c -o dma_rv32imc.o

# Link 成不同的 binary
riscv64-ld control_rv64gc.o -o control.elf
riscv64-ld compute_rv64gcv.o -o compute.elf
riscv32-ld dma_rv32imc.o -o dma.elf
```

**面試重點：**

**Q: "為什麼 NPU 用 AMP 而不是 SMP？"**

**A**: "三個關鍵原因：

1. **功耗效率**：不同任務有不同的功耗需求，AMP 可以讓輕量級任務跑在低功耗核心
2. **成本優化**：不需要每個核心都配備昂貴的向量單元
3. **性能專精**：Control core 優化延遲，Compute core 優化吞吐量，各司其職"

---

### NPU Compiler 面試必知的其他架構知識

#### 1. 記憶體層級架構 (Memory Hierarchy)

```text
CPU/NPU Core
    |
    v
L1 Cache (32-64 KB, ~1 cycle)
    |
    v
L2 Cache (256 KB - 1 MB, ~10 cycles)
    |
    v
L3 Cache (共享, 2-8 MB, ~40 cycles)
    |
    v
DRAM (GB 級, ~100-300 cycles)
```

**Compiler 需要考慮的：**

```c
// Bad: Cache thrashing
for (int i = 0; i < N; i++) {
    for (int j = 0; j < M; j++) {
        C[i][j] += A[i][k] * B[k][j];  // B 的存取不連續
    }
}

// Good: Tiling for cache locality
for (int ii = 0; ii < N; ii += TILE) {
    for (int jj = 0; jj < M; jj += TILE) {
        for (int kk = 0; kk < K; kk += TILE) {
            // 小塊矩陣乘法，fit in cache
            matmul_tile(A, B, C, ii, jj, kk, TILE);
        }
    }
}
```

**面試重點**：能說出 cache line size (通常 64 bytes)、cache associativity、false sharing 的概念。

---

#### 2. DMA (Direct Memory Access)

```text
傳統方式 (CPU copy):
CPU ──讀取──> Memory A
CPU ──寫入──> Memory B
問題：CPU 被佔用，無法做其他事

DMA 方式:
CPU ──啟動DMA──> DMA Engine
DMA Engine ──自動搬移──> Memory A -> Memory B
CPU ──繼續執行其他任務──>
問題解決：CPU 不被阻塞
```

**NPU 中的 DMA Pattern：**

```c
// Double buffering for zero overhead
Buffer A: 正在被 NPU 處理
Buffer B: DMA 正在搬入下一批資料

while (has_data) {
    // Overlap computation and data transfer
    dma_start_async(input_data, buffer_next);  // 非阻塞
    npu_compute(buffer_current);               // NPU 運算
    dma_wait(buffer_next);                     // 等待 DMA 完成
    swap(buffer_current, buffer_next);         // 交換 buffer
}
```

**面試重點**：理解 zero-copy、scatter-gather DMA、descriptor-based DMA。

---

#### 3. 中斷處理 (Interrupt Handling)

```text
NPU 完成運算
    |
    v
硬體觸發中斷 (IRQ)
    |
    v
CPU 跳到 ISR (Interrupt Service Routine)
    |
    v
讀取 NPU 狀態暫存器
    |
    v
喚醒等待的 thread
    |
    v
返回正常執行
```

**RISC-V 的中斷機制：**

```c
// 中斷處理函數
void npu_irq_handler(void) {
    uint32_t status = npu_read_status();

    if (status & NPU_DONE) {
        // 任務完成
        complete_task();
    }

    if (status & NPU_ERROR) {
        // 錯誤處理
        handle_error();
    }

    npu_clear_interrupt();
}

// 註冊中斷
register_irq(NPU_IRQ_NUM, npu_irq_handler);
```

**面試重點**：理解 interrupt vs polling、interrupt priority、nested interrupt。

---

#### 4. 同步機制 (Synchronization)

```c
// 1. Spinlock (適合短時間等待)
void spinlock_acquire(atomic_int *lock) {
    while (atomic_exchange(lock, 1) == 1) {
        // 忙等待
    }
}

// 2. Mutex (適合長時間等待)
void mutex_lock(mutex_t *m) {
    if (!trylock(m)) {
        sleep_and_wait();  // 讓出 CPU
    }
}

// 3. Semaphore (計數器，用於資源管理)
void sem_wait(sem_t *s) {
    atomic_decrement(&s->count);
    if (s->count < 0) {
        block();
    }
}
```

**NPU Task Queue 的同步：**

```c
// Producer (CPU)
void submit_npu_task(task_t *task) {
    spinlock_acquire(&queue_lock);
    enqueue(task);
    spinlock_release(&queue_lock);

    sem_post(&task_count);  // 通知有新任務
}

// Consumer (NPU control core)
void npu_worker(void) {
    while (1) {
        sem_wait(&task_count);  // 等待任務

        spinlock_acquire(&queue_lock);
        task_t *task = dequeue();
        spinlock_release(&queue_lock);

        npu_execute(task);
    }
}
```

**面試重點**：理解 atomic operations、memory ordering、lock-free 資料結構。

---

#### 5. RISC-V 特權模式 (Privilege Levels)

```text
M-mode (Machine)        最高權限
    |
    v
S-mode (Supervisor)     OS Kernel
    |
    v
U-mode (User)           User applications
```

**NPU Firmware 的模式使用：**

M-mode:

- Bootloader
- NPU 硬體初始化
- Exception handler

S-mode:

- Device driver (如果跑 Linux)
- Memory management
- Virtual memory

U-mode:

- AI runtime (TensorFlow Lite, ONNX Runtime)
- User applications

**面試重點**：理解 CSR (Control and Status Registers)、trap handling、context switch。

---

### 面試前的最後檢查清單

#### 必須能回答的問題：

1. **"RV64GC 和 RV64GCV 的差別是什麼？"**

   - 答：GC 是通用配置（整數+浮點+原子+壓縮），GCV 額外加上 Vector extension

2. **"為什麼 NPU 需要 AMP 架構？"**

   - 答：功耗優化、成本優化、性能專精化

3. **"VLA (Vector Length Agnostic) 對 compiler 的好處？"**

   - 答：統一 codegen、無需 cleanup loop、binary portability

4. **"NPU compiler 的 fallback 機制如何運作？"**

   - 答：根據運算大小和硬體能力決策，小運算用 RVV，大運算用 NPU

5. **"如何優化 NPU 的記憶體搬移？"**
   - 答：DMA + double buffering、cache locality、zero-copy

---

#### 技術名詞必須知道：

- **ISA (Instruction Set Architecture)**：指令集架構
- **VLEN**：Vector register length (128, 256, 512...)
- **LMUL**：Register grouping (m1, m2, m4, m8)
- **ELEN**：Maximum element width (32, 64 bits)
- **CSR**：Control and Status Registers
- **Fence**：Memory ordering 指令
- **ABI (Application Binary Interface)**：函數呼叫規範

---

#### 可以加分的話題：

1. 提到晶心科技（Andes）- 台灣公司
2. 了解 MLIR 的 lowering pipeline
3. 知道 LLVM RISC-V backend 的關鍵檔案
4. 能討論 polyhedral optimization
5. 理解 quantization (INT8/INT16)

---

#### 準備的程式碼範例：

```c
// 1. 簡單的 RVV 向量化
void vector_add(float *a, float *b, float *c, size_t n) {
    size_t vl;
    for (size_t i = 0; i < n; i += vl) {
        vl = vsetvl_e32m1(n - i);
        vfloat32m1_t va = vle32_v_f32m1(&a[i], vl);
        vfloat32m1_t vb = vle32_v_f32m1(&b[i], vl);
        vfloat32m1_t vc = vfadd_vv_f32m1(va, vb, vl);
        vse32_v_f32m1(&c[i], vc, vl);
    }
}

// 2. NPU task submission
void submit_matmul(tensor_t *A, tensor_t *B, tensor_t *C) {
    if (should_use_npu(A, B)) {
        npu_matmul(A, B, C);  // 送到硬體
    } else {
        rvv_matmul(A, B, C);  // RVV fallback
    }
}
```

---

### 快速複習卡

#### RV64GC

- **含義**：RV64I + M + A + F + D + C
- **用途**：Control processor, device driver
- **為什麼需要**：乘除法、原子操作、浮點運算、壓縮指令
- **典型應用**：NPU 的 control core、跑 Linux 的主處理器

#### RV64GCV

- **含義**：RV64GC + V extension
- **用途**：AI 運算、向量化處理
- **為什麼需要**：高效的 fallback 路徑、處理不值得送 NPU 的小運算
- **典型應用**：NPU 的 compute core、前後處理

#### AMP

- **含義**：Asymmetric Multi-Processing
- **vs SMP**：異構 vs 同構
- **三大優勢**：功耗優化、成本優化、性能專精
- **典型配置**：Control (RV64GC) + Compute (RV64GCV) + DMA (RV32IMC)

#### VLA (Vector Length Agnostic)

- **核心機制**：vsetvl 動態設定向量長度
- **最大優勢**：Write once, run anywhere
- **vs 固定長度**：無需 cleanup loop、binary portability
- **Compiler 好處**：統一 codegen、提高 auto-vectorization 成功率

---

**祝你面試順利！記住：展現你的學習能力和對技術的熱情，比死記硬背更重要。**

**最重要的心態：把 RISC-V 當作解決實際問題的工具，而不是抽象的概念。面試官想看到你能用這些知識解決真實的 NPU compiler 問題。**
