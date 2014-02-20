{Оберон 2} 
{http://informatika.kspu.ru/mproj/umk_modeling/lection6.php#2}
Program Ochered;
{входной поток равновероятных событий;
 динамические массивы позволяют значительно увеличить объем выборки}
Uses Crt, Graph;
Const N = 10000 {число членов выборки};
      W1 = 10 {диапазон времен прихода от 0 до w1};
      W2 = 5 {диапазон времен обслуживания от 0 до w2};
Type T = Array[1..N] Of Real; U = ^T;
Var A, B, C, D, E, F, Aa, Bb, Cc, Dd, Ee, Ff, Dg, Dh, M : Real;
    S1, S2 : Double; I, K, J, I1, I2 : Integer;
    L1, L2, V : Array [1..11] Of Real; G, H : U; Ch : Char;
Begin
    If MaxAvail >= SizeOf(G) Then New(G);
    If MaxAvail >= SizeOf(H) Then New(H);
    Randomize; {ниже - имитационное моделирование}
    Aa := 0; Bb := W2 * Random; Cc := 0; Ee := Bb; Ff := Bb;
    G^[1] := 0; H^[1] := 0;
    For K := 1 To 11 Do
      Begin L1[K] := 0; L2[K] := 0 End;
    For I := 2 To N Do
      Begin
        A := W1 * Random; B := W2 * Random;
        C := Cc + A; If C > Ee Then D := C Else D := Ee;
        E := D + B; F := E - C; G^[I] := F - B; H^[I] := D - Ee; 
        Cc := C; Ee := E;
        If G^[I] <= 1 Then L1[1] := L1[1] + 1; If H^[I] = 0 Then 
          L2[1] := L2[1] + 1;
        For K := 2 To 10 Do
        Begin
          If (G^[I] > K - 1) And (G^[I] <= K) Then L1[K] := L1[K] + 1;
          If (H^[I] > K - 1) And (H^[I] <= K) Then L2[K] := L2[K] + 1;
        End;
        If G^[I] > 10 Then L1[11] := L1[11] + 1; 
        If H^[I] > 10 Then L2[11] := L2[11] + 1;
        S1 := S1 + G^[I]; S2 := S2 + H^[I];
      End;
      For I := 1 To 11 Do {ниже - нормировка распределений g и h}
        Begin
           L1[I] := L1[I] / N; L2[I] := L2[I] / N
        End;
      {ниже - расчет средних и дисперсий величин g и h}
    S1 := S1 / N; S2 := S2 / N; Dg := 0; Dh := 0;
    For I := 1 To N Do
      Begin
         Dg := Dg + Sqr(G^[I] - S1); Dh := Dh + Sqr(H^[I] - S2)
      End;
    Dg := Dg / N; Dh := Dh / N;
    WriteLn('распределение величины g распределение величины h'); 
    WriteLn;
    For K := 1 To 11 Do
      WriteLn('l1[', K, ']=', L1[K] : 6 : 4, ' ' : 20, 'l2[', K, ']=', 
         L2[K] : 6 : 4);
    WriteLn;
    WriteLn('выборочное среднее величины g=', S1 : 6 : 3, 
    ' выборочная дисперсия величины g=', Dg : 6 : 3);
    WriteLn('выборочное среднее величины h=', S2 : 6 : 3, 
    ' выборочная дисперсия величины h=', Dh : 6 : 3);
    Dispose(G); Dispose(H); WriteLn;
    WriteLn('для продолжения нажать любую клавишу');
    Repeat Until KeyPressed; Ch := ReadKey;
      {ниже - построение гистограмм распределений величин g и h}
    DetectGraph(I, K); InitGraph(I, K, '');
    I := GetMaxX; K := GetMaxY; J := I Div 2; M := L1[1];
    For I1 := 2 To 11 Do If L1[I1] > M Then M := L1[I1];
    For I1 := 1 To 11 Do V[I1] := L1[I1] / M;
    Line(10, K - 10, J - 20, K - 10); Line(10, K - 10, 10, 5);
    OutTextXY(20, 100, 'распределение величины g');
    For I1 := 1 To 11 Do
     Begin
         I2 := Round((K - 20) * (1 - V[I1])) + 10;
         Line(I1 * 20 - 10, I2, I1 * 20 + 10, I2);
         Line(I1 * 20 - 10, I2, I1 * 20 - 10, K - 10);
         Line(I1 * 20 + 10, I2, I1 * 20 + 10, K - 10);
     End;
    Line(J + 20, K - 10, I - 10, K - 10); 
    Line(J + 20, K - 10, J + 20, 5);
    OutTextXY(J + 30, 100, 'распределение величины h'); M := L2[1];
    For I1 := 2 To 11 Do If L2[I1] > M Then M := L2[I1];
    For I1 := 1 To 11 Do V[I1] := L2[I1] / M;
    For I1 := 1 To 11 Do
     Begin
      I2 := Round((K - 20) * (1 - V[I1])) + 10;
      Line(J + I1 * 20, I2, J + I1 * 20 + 20, I2);
      Line(J + I1 * 20, I2, J + I1 * 20, K - 10);
      Line(J + I1 * 20 + 20, I2, J + I1 * 20 + 20, K - 10);
     End;
    OutTextXY(200, GetMaxY - 10, 'для выхода нажать любую клавишу');
    Repeat Until KeyPressed; CloseGraph
End.

