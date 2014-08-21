# A list is a generic vector containing other objects.
# For example, the following variable x is a list containing copies of three vectors n, s, b, and a numeric value 3.

n = c(2, 3, 5) 
s = c("aa", "bb", "cc", "dd", "ee") 
b = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x = list(n, s, b, 3)   # x contains copies of n, s, b

# List Slicing
# We retrieve a list slice with the single square bracket "[]" operator. The following is a slice containing the second member of x, which is a copy of s.
x[2] 
# [[1]] 
# [1] "aa" "bb" "cc" "dd" "ee"


# With an index vector, we can retrieve a slice with multiple members. Here a slice containing the second and fourth members of x.
x[c(2, 4)] 
# [[1]] 
# [1] "aa" "bb" "cc" "dd" "ee" 
# 
# [[2]] 
# [1] 3

# Member Reference
# In order to reference a list member directly, we have to use the double square bracket "[[]]" operator. The following object x[[2]] is the second member of x. In other words, x[[2]] is a copy of s, but is not a slice containing s or its copy.

x[[2]] 
# [1] "aa" "bb" "cc" "dd" "ee"

# We can modify its content directly.
x[[2]][1] = "ta" 
x[[2]] 
# [1] "ta" "bb" "cc" "dd" "ee" 
s 
# [1] "aa" "bb" "cc" "dd" "ee"   # s is unaffected

