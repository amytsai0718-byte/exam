! =====================================================================
! 程式名稱：sum_to_n
! 功能：計算 1 + 2 + ... + N 的總和，並用公式 N(N+1)/2 驗算。
!       先處理使用者輸入的 N，再自動進行三次隨機測試。
! 編譯：gfortran -static sum.f90 -o sum.exe
! 執行：.\sum.exe
! =====================================================================
program sum_to_n
  implicit none                       ! 所有變數都必須先宣告才能使用

  ! ----- 變數宣告 -----
  integer :: n                        ! 要加總到的上限 N
  integer :: i                        ! 加總迴圈的計數器
  integer :: t                        ! 隨機測試的次數計數器
  integer(kind=8) :: total            ! 迴圈算出的總和（8 位元組整數，避免溢位）
  integer(kind=8) :: formula          ! 公式算出的總和
  real :: r                           ! 隨機實數，範圍為 0 <= r < 1

  ! ----- 第一部分：由使用者輸入 N -----
  print *, '請輸入 N：'
  read *, n

  ! 用迴圈把 1 到 N 逐一加總
  total = 0
  do i = 1, n
     total = total + i
  end do

  ! 輸出迴圈結果與公式驗算結果
  ! int(n,8) 先把 n 轉成 8 位元組整數，避免 n*(n+1) 相乘時溢位
  print *, '1 加到 ', n, ' 的總和 = ', total
  print *, '用公式 N(N+1)/2 驗算 = ', int(n,8)*(n+1)/2

  ! ----- 第二部分：三次隨機測試，N 為 1 到 1000000 的隨機整數 -----
  print *, '--- 三次隨機測試 ---'
  call random_seed()                  ! 初始化亂數，讓每次執行抽到不同的數字
  do t = 1, 3
     ! 把 0 <= r < 1 的隨機實數換算成 1 到 1000000 的整數
     call random_number(r)
     n = 1 + int(r * 1000000.0)

     ! 用迴圈計算總和（每次測試都要先歸零）
     total = 0
     do i = 1, n
        total = total + i
     end do

     ! 用公式計算總和
     formula = int(n,8)*(n+1)/2

     ! 輸出這次測試的 N 與兩種算法的結果
     print *, '第', t, '次：N = ', n
     print *, '  迴圈總和 = ', total
     print *, '  公式驗算 = ', formula

     ! 比較兩種算法的結果是否相同
     if (total == formula) then
        print *, '  結果：通過'
     else
        print *, '  結果：不符'
     end if
  end do
end program sum_to_n
