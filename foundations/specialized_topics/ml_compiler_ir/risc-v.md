---
title: RISC-V Learning
author: Yi-Ping Pan (Cloudlet)
---

# RISC-V Learning

## TOC

(我們今天的目標是充分準備 Mediatek compiler engineer 的職缺，有內部消息透露 RISC-V 很重要．所以所有的準備)

**History of RISC-V**

- [ ] 2010年 UC Berkeley Krste Asanović 教授團隊創建
- [ ] 為什麼叫 "V"：第五代 RISC（前四代是 Berkeley 的研究專案）
- [ ] 2015年成立 RISC-V International（非營利組織）
- [ ] 2020年總部從美國搬到瑞士（避免美國出口管制）

**What does it want to solve**

- [ ] 專有 ISA 的授權費問題（ARM 每顆晶片收費）
- [ ] 供應商鎖定（vendor lock-in）
- [ ] 學術研究的法律障礙
- [ ] 架構的戰略獨立性
- [ ] 遺留包袱（x86 有40+年歷史的相容性負擔）
- [ ] 客製化困難（ARM 不讓你隨意改）

**Ecosystem overview**

- [ ] 硬體：SiFive, Andes (晶心科技), Alibaba T-Head, StarFive
- [ ] 軟體：GCC, LLVM, Linux kernel 支援
- [ ] 作業系統：Linux, FreeRTOS, Zephyr
- [ ] 開發板：HiFive, VisionFive, Milk-V
- [ ] 產業支持：Google, NVIDIA, Qualcomm, Samsung, Intel
- [ ] 中國大力投資（避免被卡脖子）
- [ ] 印度的 Shakti 處理器專案

---

**RV32I - 32-bit 基本整數指令集**

- [ ] 32個通用暫存器（x0-x31，x0 永遠是 0）
- [ ] 47條基本指令
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

- [ ] 32個浮點暫存器（f0-f31）
- [ ] IEEE 754 標準

**D Extension - 雙精度浮點（64-bit double）**

- [ ] 擴展 F Extension

**C Extension - 壓縮指令（16-bit）**

- [ ] 將常用的32-bit指令壓縮成16-bit
- [ ] 節省程式碼大小（重要！）
- [ ] 對嵌入式系統的記憶體很有幫助

**V Extension - 向量運算（Vector）** ⭐ 重點！

- [ ] 為什麼重要：AI workload 的核心
- [ ] 可變長度向量（VLEN 可以是 128, 256, 512...）
- [ ] 與 ARM NEON（固定128-bit）的差異
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
- [ ] 歷史包袱：乾淨 vs 40年相容性負擔
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

- 常用的立即數範圍 → 用壓縮指令
- 需要完整 32-bit 立即數 → 用標準指令
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
// → 更容易做 software pipelining 和 DMA 規劃
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

```
Architecture    Register Count    Avg Spill Rate (SPECint)
──────────────────────────────────────────────────────────
x86 (32-bit)         8                    ~15%
x86-64              16                     ~8%
ARM64               31                     ~3%
RISC-V              32                     ~2%
```

**對 AI Workload 的影響：**

```c
// Loop unrolling 時需要很多暫存器
for (int i = 0; i < N; i += 4) {
  float sum0 = a[i+0] * b[i+0];  // 需要暫存器
  float sum1 = a[i+1] * b[i+1];  // 需要暫存器
  float sum2 = a[i+2] * b[i+2];  // 需要暫存器
  float sum3 = a[i+3] * b[i+3];  // 需要暫存器
  // ... 更多的中間結果
}
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

# 而不是用一堆標準指令模擬
vmul.vf v1, v2, scale
vadd.vi v1, v1, zero_point
vmax.vi v1, v1, -128
vmin.vi v1, v1, 127
```

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

#### 7. 向量擴展 (RVV) 的革命性設計 ⭐

這是 RISC-V 最厲害的地方，也是為什麼它對 AI 如此重要。

**Vector Length Agnostic (VLA) 設計：**

```c
// 同樣的程式碼
void vector_add(float *a, float *b, float *c, size_t n) {
  size_t vl;
  for (size_t i = 0; i < n; i += vl) {
    vl = vsetvl_e32m1(n - i);  // 硬體告訴你這次能處理多少元素
    vfloat32m1_t va = vle32_v_f32m1(&a[i], vl);
    vfloat32m1_t vb = vle32_v_f32m1(&b[i], vl);
    vfloat32m1_t vc = vfadd_vv_f32m1(va, vb, vl);
    vse32_v_f32m1(&c[i], vc, vl);
  }
}
```

**魔法在哪？**

- **VLEN=128 的晶片**：一次處理 4 個 float (128/32=4)
- **VLEN=256 的晶片**：一次處理 8 個 float (256/32=8)
- **VLEN=512 的晶片**：一次處理 16 個 float (512/32=16)
- **同樣的二進制檔案，無需重新編譯！**

**對比其他架構：**

- **ARM NEON**：固定 128-bit，如果硬體是 256-bit 就浪費了
- **AVX-512**：固定 512-bit，如果跑在只有 256-bit 的機器就無法執行

**NPU Compiler 的實際應用：**

```python
# 在 MLIR 層面
func.func @matmul(%A: tensor<?x?xf32>, %B: tensor<?x?xf32>) {
  // Compiler 生成的向量化程式碼可以適應不同的硬體
  linalg.matmul ins(%A, %B: tensor<?x?xf32>, tensor<?x?xf32>)
                outs(%C: tensor<?x?xf32>)

  // Lower 到 RISC-V Vector
  // → 自動適應 VLEN
}
```

**Vector Mask 和 Tail Handling：**

```c
// 處理不對齊的長度（例如 n=33）
for (size_t i = 0; i < n; i += vl) {
  vl = vsetvl_e32m1(n - i);  // 最後一輪 vl 會自動變成 1
  // 硬體會自動 mask 掉多餘的元素，不需要 scalar cleanup code
  vadd_vv(...);
}
```

ARM NEON 需要手動處理剩餘元素：

```c
// NEON 版本
int i;
for (i = 0; i < n - 3; i += 4) {
  // 向量化程式碼
}
// 剩餘元素需要 scalar loop
for (; i < n; i++) {
  c[i] = a[i] + b[i];
}
```

**豐富的 Load/Store Patterns：**

```assembly
# Unit-stride load (連續記憶體)
vle32.v v1, (a0)

# Strided load (跳躍存取，用於 NCHW → NHWC 轉換)
vlse32.v v1, (a0), a1

# Indexed load (scatter/gather，用於稀疏矩陣)
vlxei32.v v1, (a0), v2

# Segment load (用於 AoS → SoA 轉換)
vlseg3e32.v v1, v2, v3, (a0)
```

這些 pattern 直接對應 AI workload 的常見需求，compiler 可以直接映射，不需要複雜的 shuffle。

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

**1. LR/SC (Load-Reserved/Store-Conditional)**

```assembly
retry:
  lr.w  x10, (x11)      # Load-Reserved
  addi  x10, x10, 1     # 修改值
  sc.w  x12, x10, (x11) # Store-Conditional
  bnez  x12, retry      # 如果失敗就重試
```

**2. AMO (Atomic Memory Operations)**

```assembly
amoadd.w x10, x12, (x11)  # Atomic add
```

**為什麼兩種？**

- **LR/SC**：更通用，可以實現任何原子操作
- **AMO**：更高效，單條指令完成，硬體可以優化

**NPU 系統的實際應用：**

```c
// Job queue 的多生產者-多消費者模型
void enqueue(job_t *job) {
  int tail;
  do {
    tail = __atomic_load_n(&queue->tail, __ATOMIC_ACQUIRE);
  } while (!__atomic_compare_exchange_n(&queue->tail, &tail, tail+1,
                                        false, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE));
  queue->jobs[tail] = job;
}
```

Compiler 會生成對應的 LR/SC 或 AMO 指令。

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
┌─────────────┐
│ NPU Cluster │
├─────────────┤
│ Compute Core│ ← RV64IMAFDCV (全功能)
│ Compute Core│
├─────────────┤
│ Control Core│ ← RV32IMC (精簡)
└─────────────┘
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
