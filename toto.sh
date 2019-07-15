git ls-tree -r -z --name-only HEAD |egrep -z -Z -E '\.(c|h)$' |xargs -0 -n1 git blame -w --line-porcelain HEAD |grep  "^author "|sort|uniq -c|sort -nr

