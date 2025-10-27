# Time Complexity

## The complexity of Loops

Definition: **Estimation** of total **CPU computations** required to execute a loop

Why loops?
Ans: In any types of algoritm, loops contributes a significant amout of time complexity

Observe the **frequency count** of the **innermost** instruction of a loop

### Single Loops

#### Update expression with increment

##### Eample 1: Trivial example

```cpp
for (int i = 0; i < n; i++) {
    printf("Cloudlet");
}
```

"Cloudlet" is printed n times --> O(n)

##### Example 2: Increment by const

```cpp
for (int i = 0; i < n; i = i + 2) {
    printf("Cloudlet");
}
```

print while i = 0, 2, 4, 6 ...

"Cloudlet" is printed n/2 times --> O(n)

Example 2: Increment update expression const - 2

```cpp
for (int i = 0; i < n; i = i + 10) {
    printf("Cloudlet");
}
```

#### Update expresson with multiply or divide

##### Example 3: Incremenat update expression by const

```cpp
for (int i = 0; i < n; i = i + 2) {
    printf("Cloudlet");
}
```

print while i = 0, 2, 4, 6 .. n/2

"Cloudlet" is printed (n/10 + 1) times --> O(n)

##### Example 4: Multiply update expression by const

```cpp
for (int i = 1; i < n; i = i * 2) {
    printf("Cloudlet");
}
```

print while 1, 2, 4, 8 ... 2^k

where 2^k = n
=> k = log2(n)

So, print log2(n) + 1 times => O(log(n))

##### Example 5: Divide update expression by const

```cpp
for (int i = n; i < 1; i = i / 2) {
    printf("Cloudlet");
}
```

print while n, n/2, n/4, ... n/2^k
where n/2^k = 1
=> n = 2^k
=> k = log2(n)

So, print log2(n) + 1 times ==> O(log(n))

#### Update Expression with Power

##### Example 6:

```cpp
for (int i = 2; i < n; i = i * i) {
    printf("Cloudlet");
}
```

2, 4, 16, 256, ...

2, 2^2, 2^2^2, 2^2^3, 2^2^4, ... 2^2^k,
where 2^2^k = n
=> log2(2^k) = log2(n)
=> k = log2(log2(n))

So, print log2(log2(n)) times ==> O(loglog(n))

#### Condition Statement with a fuction

##### Example 7

```cpp
for (int i = 2; i < power(2,n); i = i * i) {
    printf("Cloudlet");
}
```

2, 4, 16, ....

where 2^2^k = 2^n,
=> 2^k = n
=> k = log2(n) times
=> O(log(n))

### Nested Loops
