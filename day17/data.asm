  0   2   4  bst A     // B <- A
  2   1   3  bxl 3     // B <- B ~ 3
  4   7   5  cdv B     // C <- A / (2 ^ B)
  6   0   3  adv 3     // A <- A / (2 ^ 3)
  8   1   5  bxl 5     // B <- B ~ 5
 10   4   4  bxc       // B <- B ~ C
 12   5   5  out B     // OUT <- B
 14   3   0  jnz 0     // jump if a == 0
