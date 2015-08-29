// 問題
// 10未満の自然数のうち、3 もしくは 5 の倍数になっているものは 3, 5, 6, 9 の4つがあり、 これらの合計は 23 になる。
// 同じようにして、1,000 未満の 3 か 5 の倍数になっている数字の合計を求めよ。

def multiOf3or5(n: Int) : List[Int] =  Range(1,n).filter(m => m % 3 == 0 || m % 5 == 0).toList

multiOf3or5(1000).sum