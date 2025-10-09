---
author: Yi-Ping Pan (Cloudlet)
date: 2025-10-08
---

# From Platters to Pixels: A Deep Dive into Full System Backup

TODO: Real title should edited later.

> Before learning about backup we should understand how data is stored on a computer.

Data is stored in files and folders (directory). A file is a collection of data that is stored on a computer. A folder is a container that holds files and other folders.

Data are physically stored on a storage device such as a hard drive, solid state drive, or cloud storage. Let's learn about HDD first.

## How Data is Stored on HDD?

An explanatory framework to understand the levels of abstraction in a storage stack.

| Layer | Name                    | Description                                       |
| ----- | ----------------------- | ------------------------------------------------- |
| 1     | Physical Disk Operation | Raw hardware: platters, heads, tracks, sectors    |
| 2     | LBA Abstraction         | Linear block addressing hides physical complexity |
| 3     | Partitioning & Boot     | MBR/GPT divides disk into logical drives          |
| 4     | File System             | Manages files, folders, and metadata              |

### Layer 1: Physical Disk Operation (Traditional HDD Example)

We'll start with traditional mechanical hard disk drives (HDDs) as our model because their physical structure is the most intuitive.

Imagine an HDD like a record player:

**Platter**: Multiple metal discs coated with magnetic material, stacked together and spinning at high speed. Data is written on these platters.

**Read/Write Head**: A tiny probe that hovers above the platter surface, responsible for reading and writing magnetic signals (0s and 1s).

**Track**: Concentric circles drawn by the head on the platter surface, like race tracks.

**Sector**: Each track is divided into many small wedge-shaped sections. This smallest storage unit is called a "sector". A sector typically holds **512 bytes** or **4KB** of data. Sectors are the minimum physical read/write unit for hard drives.

**Key Point**: To the hard drive hardware itself, it doesn't understand "files" - it only knows "write this data to platter X, track Y, sector Z."

### Layer 2: Operating System Abstraction - LBA

If the operating system (OS) had to directly manage which data goes on which platter, it would be too complex. So, the hard drive's **controller** provides a crucial abstraction layer:

**Logical Block Addressing (LBA)**

LBA abandons all the complex physical structure and imagines the entire hard drive as a simple, linear array of **blocks** numbered from 0.

LBA 0, LBA 1, LBA 2, LBA 3, ... all the way to the last block of the hard drive.

The OS only needs to tell the controller: "Please write data to LBA 500" or "Please read data from LBA 1024."

As for which platter and track LBA 500 actually corresponds to, that's the hard drive controller's job - the OS doesn't need to care.

**Analogy**: Just like when you write C++ programs, you work with continuous virtual memory addresses without worrying about which physical RAM bank they map to. LBA is to disks what virtual memory is to RAM.

### Layer 3: Disk "Map" - Partitioning and Boot Sectors

Now we have a huge, linear block space. But we usually don't use the entire space as one unit - instead, we divide it into C: drive, D: drive, etc. This process is called **disk partitioning**.

> How does the computer know how this hard drive is divided? Where does each partition start and end?

The answer lies at the very beginning of the hard drive (LBA 0):

**Master Boot Record (MBR)** or **GUID Partition Table (GPT)**:

| Feature                  | MBR (Master Boot Record) | GPT (GUID Partition Table)                |
| ------------------------ | ------------------------ | ----------------------------------------- |
| **Year Introduced**      | 1983 (IBM)               | 2005 (UEFI Specification)                 |
| **Maximum Disk Size**    | 2 TB                     | 9.4 ZB (practically unlimited)            |
| **Maximum Partitions**   | 4 primary partitions     | 128 partitions (default)                  |
| **Boot Support**         | BIOS only                | UEFI (and BIOS with compatibility)        |
| **Redundancy**           | Single copy at LBA 0     | Primary + backup copy                     |
| **Partition Table Size** | 64 bytes                 | Variable (32 KB default)                  |
| **OS Compatibility**     | All operating systems    | Modern OS (Windows 7+, Linux 2.6+, macOS) |

This is a "disk partition map" stored at the most important location: LBA 0.

It records: "C: drive starts at LBA 2048, size 500 GB; D: drive starts at LBA XXXXX, size 1 TB."

It also contains the primitive boot loader code that tells the computer's BIOS/UEFI which partition to find the operating system from.

**Key Point**: If the MBR/GPT is corrupted, even if all the data behind it is intact, the operating system cannot find its home and the computer cannot boot.

### Layer 4: File "Manager" - File System

After partitioning, we get chunks of "land" (partitions), but we still need a management system to "build houses, pave roads, and assign addresses" on this land. This manager is the **File System**.

Common file systems include Windows' **NTFS**, Linux's **ext4**, and macOS' **APFS**.

When you create a file (e.g., `note.md`), the file system does several things:

1. Finds some empty LBA blocks to store the content data of `note.md`.
2. In a special area called "**Metadata**" (think of it as the file system's index table), it records:
   - The filename is `note.md`
   - File size, creation time, permissions, etc.
   - **Most critically**: This file's content is stored in blocks LBA 1234, LBA 1235, LBA 2567, etc.

> So... File systems have two main components: **metadata structures** + **data blocks**.
> The metadata structures (like inodes, directory entries, and allocation tables) form tree-like hierarchies that describe the directory structure and track which data blocks belong to each file. These metadata structures then point to the actual data blocks containing file content.

When you open `note.md`, the file system looks up its metadata, finds all the LBA blocks where the content is stored, and reads them in order to reconstruct the complete file content.

**Other resources**:

- [kernel.org - Filesystems in the Linux kernel](https://docs.kernel.org/filesystems/index.html)
- [Jserv - Linux File System (zh-TW)](https://hackmd.io/@sysprog/linux-file-system)
- [Understanding the Linux Filesystem: An In-Depth Guide for DevOps Engineers](https://dev.to/prodevopsguytech/understanding-the-linux-filesystem-an-in-depth-guide-for-devops-engineers-ona)
- [File Descriptors and File Systems](https://man7.org/training/download/lusp_fileio_slides.pdf)

### My Questions

#### What happens when physical segments break?

When a physical sector becomes damaged, the **hard drive controller** handles this automatically:

**Bad Sector Remapping**: Modern drives maintain a **spare sector pool**. When the controller detects a bad sector during read/write operations, it:

1. Marks the physical sector as unusable
2. Remaps that LBA to a healthy spare sector
3. Updates its internal translation table
4. The OS continues to see the same LBA numbers - it's completely transparent

**Example**:

- LBA 1000 originally mapped to physical sector on Platter 2, Track 50, Sector 10
- That physical sector fails
- Controller remaps LBA 1000 to a spare sector on Platter 3, Track 100, Sector 5
- OS still reads/writes to "LBA 1000" without knowing anything changed

**S.M.A.T.T. Monitoring**: Drives track reallocated sectors and warn when spare sectors are running low. (Self-Monitoring, Analysis, and Reporting Technology.)

**Key Point**: This is why LBA abstraction is so powerful - physical hardware failures are handled invisibly by the controller layer, maintaining the illusion of continuous, reliable block storage for the OS.

> So now we can start talking about backups.

## How Full System Backup Works?

Now that we understand the four layers of disk storage, we can explain how full system backup operates at each level:

### Block-Level vs File-Level Backup

**File-Level Backup** (Layer 4 - File System):

- Reads files through the file system (NTFS, ext4, APFS)
- Understands folder structure, permissions, metadata
- Only backs up files that exist and are accessible
- **Problem**: If the file system is corrupted, files become inaccessible even if the data blocks are intact

**Block-Level Backup** (Layer 2 - LBA):

- Reads raw LBA blocks directly from the hard drive controller
- Ignores file system structure completely
- Creates a **sector-by-sector copy** of the entire disk
- **Advantage**: Can recover from file system corruption, partition table damage, or boot loader issues

### What Gets Captured in Full System Backup?

A full system backup captures **everything** at the LBA level:

1. **Layer 3 - Partition Information**:

   - Complete MBR/GPT partition table
   - Boot loader code
   - Partition boundaries and types

2. **Layer 4 - All File Systems**:

   - System files (Windows/, Linux /boot, macOS /System)
   - User data (Documents, Pictures, etc.)
   - Hidden files and system metadata
   - Registry (Windows) or configuration files (Linux/macOS)

3. **Layer 2 - Raw Block Data**:
   - Even "deleted" files that haven't been overwritten
   - Unused space between partitions
   - Bad sector remapping information

### The Backup Process

```text
Original Disk:    [MBR][Partition 1: OS][Partition 2: Data][Unallocated]
                    ↓ Block-by-block copy ↓
Backup Image:     [MBR][Partition 1: OS][Partition 2: Data][Unallocated]
```

**Raw Disk Access Process for Full System Backup**:

1. **Permission Request**: Backup software (running with **administrator privileges**) makes a request to the low-level **storage stack** of the operating system, such as the **Volume Manager**: _"Please grant me raw access to the C: drive. I need to read starting from LBA 0."_

2. **Authorization**: After verifying the permissions, the operating system **bypasses** the file system logic entirely and directly authorizes the software to read the partition's **binary stream**.

3. **Raw Data Reading**: The backup software operates like executing a `dd` command - it sequentially reads the raw binary data from LBA 0, 1, 2, 3... from beginning to end and writes it to an **image file** (.img, .iso, .vhd, .vmdk).

4. **Block-Level Operation**: Throughout this process, the backup software operates on **disk blocks**, not **files**.

5. **Result**: Complete bit-for-bit copy of the entire disk with compression applied.

**Key Point**: This process completely bypasses Layer 4 (File System) and works directly at Layer 2 (LBA), which is why it can capture everything including corrupted file systems, deleted data, and boot sectors.

## System Level Challenges of Full System Backup

1. How to backup a "running" system?
2. How to minimize performance impact on the live system during backup? (Intremental backup)
3. How to revoverver from a full system backup image to a new system?
4. How to perform single file recovery from a full system backup image?

### How to Backup a "Running" System?

If we directly read LBA blocks sequentially on a running system, **catastrophic consequences** would occur.

Imagine this scenario: while you're halfway through the backup, the operating system updates a database file. Your backup might capture the "before update" first half of the file and the "after update" second half. This backup would be **corrupted and inconsistent** - the restored system would guaranteed crash.

#### The School Photography Analogy

**Cast and Setting**

- **The Entire School**: Your complete hard drive (`/dev/sda`)
- **Individual Classrooms**: LBA blocks on the disk. Imagine the school as an infinitely long hallway with classroom numbers 0, 1, 2, 3... stretching down the corridor
- **Students in Classrooms**: Binary data stored in each LBA block
- **Students' Clothing**: The state of data at a specific point in time
- **The Principal**: Linux Kernel - the school administrator who knows everything
- **You**: The yearbook photographer (backup administrator)

**Your Mission**: Create a perfect "10:00 AM Sharp" yearbook that precisely captures every classroom's state at exactly 10:00:00 - which student is in which room and what they're wearing.

##### The Wrong Approach: Room-by-Room Photography

After receiving your assignment, you think it's simple - just start from "Classroom 0" and work your way through:

- 10:00:00 AM: You snap the first photo of "Classroom 0"
- 10:00:01 AM: You move to "Classroom 1" and take the second photo
- ...
- 10:08:20 AM: You finally reach "Classroom 500"

**Here's the problem**: The school didn't stop operating just because you're taking pictures!

**Class period ends at 10:05 AM!** Tommy, who was in "Classroom 10" wearing a red shirt at 10:00, has now moved to "Classroom 300" for PE and changed into a blue gym uniform.

**What does your yearbook (backup image) look like?**

- **Front section**: A photo taken at 10:00:10 showing Tommy in "Classroom 10" wearing red
- **Back section**: A photo taken at 10:06:40 showing Tommy in "Classroom 300" wearing blue
- **Worse yet**: Sarah, who was originally in "Classroom 300" at 10:00 sharp, got displaced when Tommy arrived. Your photos completely missed Sarah's 10:00 AM state!

**Conclusion**: This yearbook is a disaster - people, locations, and times are completely mixed up. This is exactly what happens when you directly `dd` copy a running system. The backup is corrupted.

##### The Right Approach: Ask the Principal for Help

You realize that going room-by-room won't work. So you approach the school administrator - the **Principal (Linux Kernel)**.

You tell the Principal: "I need to create a perfectly accurate '10:00 AM Sharp' school-wide yearbook."

The Principal nods and reveals his "secret weapon" - **LVM Snapshots**.

**Step 1: Freeze**

At 09:59:59 AM, the Principal announces over the PA system (`fsfreeze`):
_"Attention all students and staff: Please maintain your current position and stay completely still for one second!"_

**Step 2: Create Snapshot - Time Magic**

At exactly 10:00:00 AM, the Principal presses a red button in his office.

This button doesn't deploy ten thousand photographers simultaneously (that would waste too many resources). Instead, it performs "time magic":

**A perfect "Mirror School" is instantly created** - identical to the real world in every detail.

This mirror school perfectly replicates the state of every classroom and every student at exactly 10:00:00 AM. It's like a time slice frozen in amber.

**Step 3: Thaw**

At 10:00:01 AM, one second after the magic, the Principal immediately announces:
_"Alright, everyone resume normal activities!"_

The real school starts operating again - students continue moving between classes. The entire freeze lasted just one second, completely unnoticed.

**Step 4: Leisurely Photography in the "Mirror School"**

Now the Principal hands you a key: _"This key accesses the frozen 'Mirror School'. Take your time photographing inside - it doesn't matter how long you take, time is permanently stopped in there."_

You enter this mirror school and can spend three hours calmly photographing every classroom. Every photo you take will perfectly reflect the 10:00:00 AM state.

##### What is Copy-on-Write?

You might wonder: if students change classrooms in the real school, what happens to the mirror school?

This is the Principal's second magic, executed by his assistant **"The Record Keeper" (Device Mapper Driver)**:

At 10:30 AM, when Tommy in the real world wants to move from "Classroom 10" to "Classroom 50":

Before Tommy can leave, the Record Keeper immediately rushes to the mirror school and posts a seal on "Classroom 10's" door reading: _"This classroom state is locked - permanently preserving Tommy's 10:00 AM appearance in red clothing."_

Only after this is done can real-world Tommy leave.

**This is CoW**: Only when a real-world classroom (LBA block) is about to change state (be written to) does the system copy and preserve the old state in the mirror school (snapshot storage). If a classroom has no activity all day, no extra work is needed.

##### Summary

The key to full system backup isn't how fast your photography skills are (`dd` speed), but whether you have permission to ask the Principal (Kernel) to use his time-freeze magic (LVM Snapshot) to create a mirror school (Snapshot Device) where you can work safely.

**The Solution: Copy-on-Write Snapshots**

Linux philosophy differs from Windows. Windows **VSS** is a highly integrated, framework-based "one-stop" service. Linux provides a series of independent, powerful, composable tools and kernel features that let us build the exact same backup workflow.

The core of this workflow is **LVM (Logical Volume Manager)** and its underlying kernel framework **Device Mapper**.

### How to minimize performance impact on the live system during backup?

The key is we

1. Skip the unsued space

   - **Challenge:**
     We need to know how to interpret the filesystem metadata (e.g., NTFS, ext4) to identify which blocks are actually used by files and which are free space.

2. **Incremental Backup: Only Backup Changed Blocks**

   The second optimization strategy focuses on backing up only the blocks that have changed since the last backup, dramatically reducing backup time and storage requirements.

   #### **Changed Block Tracking (CBT)**

   Changed Block Tracking works by maintaining a bitmap that records which blocks have been modified since the last backup. This is implemented by installing a kernel driver or module that intercepts all write operations to the disk.

   **Why Use a Bitmap?**

   Bitmaps are extremely space-efficient for tracking block changes. Each bit represents a single block's modification state: 1 for changed, 0 for unchanged. For example, tracking 1 million blocks requires only 1 million bits = 125KB of memory.

   **Why Require Kernel-Level Implementation?**

   Only the kernel has the capability to intercept all write operations to the disk. User-space programs cannot monitor every write operation, making kernel-level tracking essential for accurate change detection.

   #### **Backup Storage Models**

   **Traditional "Main + Patches" Model:**

   ```text
   Day 1 (Full)  main_backup.full      (100GB)
   Day 2 (Inc)   patch_2025_10_08.inc  (500MB)
   Day 3 (Inc)   patch_2025_10_09.inc  (300MB)
   ...
   Day 30 (Inc)  patch_2025_11_06.inc  (1GB)
   ```

   **Limitation: Fragile Restore Chain**

   This approach creates a dependency chain where restoring requires the full backup plus all incremental patches in sequence. If any patch is corrupted or missing, the entire restore fails.

   **Modern Approach: Hash Array Mapped Trie (HAMT)**

   Modern backup systems use structures similar to Haskell's Hash Array Mapped Trie, which maintains a mapping from Logical Block Addresses (LBA) to actual data blocks with structural sharing.

   **Advantages:**

   - **Robust Restore Chain**: Each backup is self-contained with references to shared blocks
   - **Fast Restore Time**: Direct access to any backup point without chain dependencies
   - **Efficient Storage**: Deduplication through structural sharing of unchanged blocks

   #### **Implementation Challenges**

   The primary challenge lies in implementing a reliable mechanism to track block changes. This requires:

   1. **Deep Filesystem Knowledge**: Understanding the internal structures of filesystems (NTFS, ext4, APFS) or volume managers
   2. **Kernel Driver Development**: Writing kernel-level code to intercept and log write operations
   3. **Change Log Management**: Maintaining persistent bitmaps or change logs that survive system restarts

FIXME: Start

#### Linux vs Windows VSS Component Mapping

| VSS (Windows) Role               | Linux Equivalent                   | Description                                                                             |
| -------------------------------- | ---------------------------------- | --------------------------------------------------------------------------------------- |
| **VSS Service** (Coordinator)    | Backup software/script             | In Linux, typically backup software or admin scripts act as the orchestrator            |
| **Writer** (Applications)        | Database applications              | e.g., MySQL, PostgreSQL - they need to be told "prepare for backup"                     |
| **Provider** (Snapshot provider) | Kernel **Device Mapper** framework | The actual low-level kernel technology that creates snapshots; LVM is the frontend tool |
| **Requestor** (Requester)        | Backup software/script             | The commander that initiates the backup workflow                                        |

#### Complete Linux Backup Workflow

**Scenario**: Backup a running Linux server with root directory on LVM volume `/dev/vg_main/lv_root`.

##### Step 1: Quiesce - Application and Filesystem Freeze

This corresponds to VSS "Freeze" phase, but in Linux typically consists of two parts:

**1.1 Notify Applications to Enter Consistent State**

The backup script's (Requestor) first task is to notify critical applications (Writers) like databases to **flush** in-memory data to disk.

This has no unified command - requires application-specific operations:

**MySQL**:

```sql
FLUSH TABLES WITH READ LOCK;
```

Ensures all tables are written to disk and temporarily locked.

**PostgreSQL**:

```sql
SELECT pg_start_backup('backup_label');
```

Enters backup mode.

This step ensures **application-level data consistency**.

**1.2 Freeze Filesystem Write Operations**

For ultimate stability, the backup script calls the `fsfreeze` tool:

```bash
fsfreeze --freeze /
```

**Kernel-level operation**: `fsfreeze` sends a special `ioctl()` system call (`FIFREEZE`) to the filesystem (e.g., ext4 or XFS). Upon receiving this command, the filesystem driver **suspends all new write operations**, queuing them. This effectively blocks all write traffic at the **VFS (Virtual File System)** layer.

⚠️ **This freeze window must be extremely short**.

##### Step 2: Create Snapshot - Kernel Copy-on-Write Performance

At the moment the filesystem is frozen by `fsfreeze`, the backup script immediately executes the most critical step:

```bash
lvcreate --snapshot --name root_snapshot --size 10G /dev/vg_main/lv_root
```

**Kernel-level operation** (Linux "Provider" at work):

1. `lvcreate` tool sends `ioctl()` system calls to the kernel's **Device Mapper** framework requesting snapshot creation.

2. **Device Mapper** completes the following operations almost instantly:
   - Creates a new virtual block device `/dev/vg_main/root_snapshot`
   - Establishes a **CoW (Copy-on-Write)** area (our allocated 10G space) and an **Exception Table** to track changes
   - From this nanosecond forward, a **read-only, time-frozen view** pointing to the original volume (`lv_root`) is born.

##### Step 3: Thaw - Resume System Operation

After successful snapshot creation, the freeze state must be immediately released:

```bash
fsfreeze --unfreeze /
```

**Kernel-level operation**: `fsfreeze` sends another `ioctl()` call (`FITHAW`) to the filesystem, telling it to release the previously suspended write queue and resume all normal I/O operations.

Simultaneously, the backup script notifies databases and other applications to unlock and resume service.

💡 **The entire freeze window**, from `fsfreeze --freeze` to `fsfreeze --unfreeze`, should ideally complete within **1 second**, completely transparent to users.

##### Step 4: Backup from Snapshot - Safe, Leisurely Data Copy

Now our backup script enters the safest, most relaxed phase:

```bash
dd if=/dev/vg_main/root_snapshot of=/mnt/backup/backup.img bs=4M
```

**Kernel-level operation**:

1. `dd` begins reading LBA blocks from `/dev/vg_main/root_snapshot` device.

2. All read requests are processed by **Device Mapper**. Since our read target is the snapshot, Device Mapper strictly follows **CoW table rules**, returning only original data belonging to the snapshot creation moment.

3. Meanwhile, users might be frantically writing new files to `lv_root`. With each write, Device Mapper silently performs **CoW operations** in the background, moving old data to the snapshot area to protect our snapshot view's integrity.

4. `dd` can take hours to complete its work, **never worrying about source data changes**. It reads from an absolutely static point in time.

**Cleanup**: After backup completion, delete the snapshot and release resources:

```bash
lvremove /dev/vg_main/root_snapshot
```

#### Key Insight

This entire workflow, though composed of different tools (`fsfreeze`, `lvcreate`, `dd`, `lvremove`), shares the same core principle as **Windows VSS**: **creating a static point-in-time copy through kernel-level Copy-on-Write mechanisms**.

### Restoration Process (Simplified)

**Complete Disk Restoration**:

- Boot from external media (USB, CD)
- Write the image file back to LBA blocks in order
- Result: **Identical disk** with same partitions, boot loader, OS, and files

**Key Advantage**: Since we captured everything at the LBA level, the restored system boots **exactly** as it was, including:

- Operating system state
- Installed programs
- User settings and documents
- Even the exact same free space pattern
  FIXME: End

## What is an "Image File" or "Disk Image"?

## What is Full System Backup vs File Backup?
