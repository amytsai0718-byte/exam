program sum_to_n  ! 程式開始，程式名稱為 sum_to_n
  implicit none  ! 關閉隱含型別，所有變數都必須先宣告
  integer :: n, i, t  ! 宣告整數：n 是加總上限，i 是加總計數器，t 是測試次數計數器
  integer(kind=8) :: total, formula  ! 宣告 8 位元組整數以避免溢位：total 是迴圈總和，formula 是公式結果
  real :: r  ! 宣告實數 r，用來存放 0 <= r < 1 的隨機數

  print *, '請輸入 N：'  ! 在螢幕上提示使用者輸入 N
  read *, n  ! 從鍵盤讀入 N，存進 n
  total = 0  ! 總和歸零，準備開始累加
  do i = 1, n  ! 迴圈開始：i 從 1 跑到 n
     total = total + i  ! 把目前的 i 加進總和
  end do  ! 迴圈結束
  print *, '1 加到 ', n, ' 的總和 = ', total  ! 輸出迴圈算出的總和
  print *, '用公式 N(N+1)/2 驗算 = ', int(n,8)*(n+1)/2  ! 輸出公式結果；int(n,8) 先轉成 8 位元組整數以避免相乘溢位

  ! 三次隨機測試：N 為 1 到 1000000 的隨機整數
  print *, '--- 三次隨機測試 ---'  ! 輸出隨機測試的標題
  call random_seed()  ! 初始化亂數，讓每次執行抽到不同的數字
  do t = 1, 3  ! 測試迴圈開始：t 從 1 跑到 3，共測三次
     call random_number(r)  ! 產生一個 0 <= r < 1 的隨機實數，存進 r
     n = 1 + int(r * 1000000.0)  ! 把 r 換算成 1 到 1000000 的整數，當作這次的 N
     total = 0  ! 這次測試的總和先歸零
     do i = 1, n  ! 加總迴圈開始：i 從 1 跑到 n
        total = total + i  ! 把目前的 i 加進總和
     end do  ! 加總迴圈結束
     formula = int(n,8)*(n+1)/2  ! 用公式 N(N+1)/2 計算總和，存進 formula
     print *, '第', t, '次：N = ', n  ! 輸出這是第幾次測試，以及抽到的 N
     print *, '  迴圈總和 = ', total  ! 輸出迴圈算出的總和
     print *, '  公式驗算 = ', formula  ! 輸出公式算出的總和
     if (total == formula) then  ! 判斷兩種算法的結果是否相同
        print *, '  結果：通過'  ! 相同時，輸出通過
     else  ! 不相同時，執行下面這行
        print *, '  結果：不符'  ! 輸出不符
     end if  ! 判斷結束
  end do  ! 測試迴圈結束
end program sum_to_n  ! 程式結束
