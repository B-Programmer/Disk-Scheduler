(* A PASCAL - LANGUAGE IMPLEMENTATION OF SSTF DISK SCHEDULING ALGORITHM*)
PROGRAM SSTFDiskScheduling(Input, Output);
uses dos;
Const
	MAX  = 199;
 	MIN  = 0;
 	SIZE = 50;

Type 
	Disk = ARRAY[1..SIZE] OF INTEGER;
Var
	n, h, count, i  : integer;
     A, T 	: Disk; 
  	AST		: Real;
    hours, minutes, seconds, milliseconds: word;
    seconds_count : longint;
    c_hours, c_minutes, c_seconds, c_milliseconds: word;

function getMinDist(d : Disk; N : integer): integer;
var  i, tmp : integer;
begin
    tmp := 1;
    for i := 2 to N do
    begin
          if(d[tmp] > d[i]) Then tmp := i
    end;
    getMinDist := tmp
end;
        	

PROCEDURE SSTF(a : Disk; Var t : Disk; N : integer; h : integer; var cnt : integer; var ast : real);
Label 30;
Var  
        i, k, J, L, LCnt, Cont, locFound : integer;
		Loc, d : Disk;
Begin     
     
	 J := 1; L := 1; LCnt := 1; 
     for k := 1 to N do Loc[k] := -1;
     for Cont := 1 to N Do
     Begin
     for i := 1 to N do
     Begin
           locFound := 0;
           for k := 1 to N  do
           Begin
                 if(i = Loc[k]) Then begin
                 locFound := 1;
                  goto 30;
                  end
           end;(*end for loop *)
30:
           if(locFound = 1) Then d[i] := 200
           else
           d[i] := abs(h - a[i])
     End; (*end of for loop *)
     L := getMinDist(d, N); 
     Loc[LCnt] := L; LCnt :=  LCnt + 1;
      t[J] := a[L]; 
     cnt := cnt + abs(h - t[J]);
     h := t[J]; 
	 J := J + 1;    
     End;(*end while *)
     ast := cnt / N;
End;	 

PROCEDURE display(a : Disk; N : integer);
VAR
     i : integer;
begin
    for i := 1 to N do
    Write(a[i]: 4);
    Writeln	
end;
Begin
   GetTime(hours, minutes, seconds, milliseconds);
   count := 0;
  writeln('*********** SSTF DISK SCHEDULING ALGORITHM IN PASCAL *****************');
  Repeat
  Writeln('Read in the total number of cylinders between 1 and 50: N ');
  readln(n);
  Until((n >= 1) AND (n <= 50));
  Writeln('Read in ', n, '  disk cylinder numbers or track locations');
  for i := 1 to n do
  Begin
   Repeat
   Write('Read in disk cylinder number or track location ', i, ', between 0 and 199: ');      
   Readln(A[i]);  
   Until((A[i] >= MIN) AND (A[i] <= MAX));
  End;
   Repeat
   Write('Read in the initial disk head position between 0 and 199: ');      
   Readln(h);  
   Until((h >= MIN) AND (h <= MAX));
  (*perform SSTF on the Cylinder numbers or track locations *)
  SSTF(A, T, n, h, count, AST);
  Writeln('The original list is: '); display(A, n);
  Writeln('The New(SSTF) list is: '); display(T, n);
  Writeln('The total head movement/seek time is: ', count);
  Writeln('The Average head movement/seek time is: ', AST:6:2);
  GetTime(c_hours, c_minutes, c_seconds, c_milliseconds);
  seconds_count := c_seconds - seconds + (c_minutes - minutes) * 60 + (c_hours - hours) * 3600;
  Writeln('The Execution time is: ',seconds_count:4,'seconds.');
  Writeln('Press any key to continue');
  readln;
End.
