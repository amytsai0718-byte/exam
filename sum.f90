program sum_to_n
  implicit none
  integer :: n, i, t
  integer(kind=8) :: total, formula
  real :: r

  print *, '請輸入 N：'
  read *, n
  total = 0
  do i = 1, n
     total = total + i
  end do
  print *, '1 加到 ', n, ' 的總和 = ', total
  print *, '用公式 N(N+1)/2 驗算 = ', int(n,8)*(n+1)/2

  ! 三次隨機測試：N 為 1 到 1000000 的隨機整數
  print *, '--- 三次隨機測試 ---'
  call random_seed()
  do t = 1, 3
     call random_number(r)
     n = 1 + int(r * 1000000.0)
     total = 0
     do i = 1, n
        total = total + i
     end do
     formula = int(n,8)*(n+1)/2
     print *, '第', t, '次：N = ', n
     print *, '  迴圈總和 = ', total
     print *, '  公式驗算 = ', formula
     if (total == formula) then
        print *, '  結果：通過'
     else
        print *, '  結果：不符'
     end if
  end do
end program sum_to_n
