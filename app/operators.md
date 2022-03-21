# C++ 快速入门
## 运算符专题
之前我们已经讲解过[变量与运算符](https://steambird1.github.io/app/2.html "变量与运算符")。这期内容，我们将列出更多的运算符及其用法：

<font color="red">下方有一些注释，请注意阅读。</font>

|  运算符 | 其它方式  | 用法  | 举例  |
| :------------: | :------------: | :------------: | :------------: |
| `(` 与 `)`  |   | 表明运算顺序 | |
| `+`  |   | 将两个数相加，或拼接两个 `string` 字符串  |  `a + 3` |
| `-`  |   | 将两个数相减 |  `a - 3` |
| `*`  |   | 将两个数相乘 |  `a * 3` |
| `/`  |   | 将两个数相除[1] |  `a / 3` |
| `%`  |   | 将两个数取余数[2] |  `a % 3` |
| `+=`  |   | `a += b` 等价于 `a = a + b`  | `a += 3`  |
| `-=`  |   | `a -= b` 等价于 `a = a - b`  | `a -= 3`  |
| `*=`  |   | `a *= b` 等价于 `a = a * b`  | `a *= 3`  |
| `/=`  |   | `a /= b` 等价于 `a = a / b`  | `a /= 3`  |
| `%=`  |   | `a %= b` 等价于 `a = a % b`  | `a %= 3`  |
| `&&`[3] | `and` | 仅当两个 `bool` 均为 `true` 时为 `true` | `a && b` |
| &#124;&#124; [3] | `or` | 两个 `bool` 中一个为 `true` 时为 `true` | a &#124;&#124; b |
| `!`[3] | `not` | 当 `false` 时为 `true`，反之亦然 | `!a` |
| `==` | | 返回 `bool`，若两个数相等则为 `true`，反之为 `false` | `a == b` |
| `!=` | | 返回 `bool`，`a != b` 等价于 `!(a == b)` | `a != b` |

### 其它的比较运算符
这些比较运算符看形状就可以知道它们的含义：
`> < >= <=`
它们和上文提到的 `== !=` 一样，都返回 `bool` 类型。
但是要注意，形如对整型变量`a`,`b`,`c`（其它类型也一样）的`a >= b >= c`的运算达不到预定效果。
这个式子中会先计算 `a >= b`，从而得到 `bool` 值参加运算，相当于 `(a >= b) >= c` [4]。

### 理解 a = a + 1
上文我们提到了 `a += 1` 等同于 `a = a + 1`。
这里可能有些难理解。其实，`a = a + 1` 只是**先计算等号右侧再给等号左侧赋值**。可以理解为等价于：
```c++
int b = a + 1;
a = b;
```


------------
这些就是本期的内容了，感谢阅读！

------------



### 注释
[1] 这里有一个精度问题：会取精度最高的。因此如果在除法的例子中`a`是`int`或者其它整数类型，其会**向下取整**。
[2] 浮点类型（如 `float`、`double`）不支持此操作。
[3] 与 `&`、`|`等不同（**这些根据二进制位进行*位运算***，以后会讲解。）另外，这些操作将两侧转为`bool`。非`0`数在转化时均为`true`，`0`为`false`。
[4] 此处将 `bool` 转换为 `int` 参与计算，你发现了吗？