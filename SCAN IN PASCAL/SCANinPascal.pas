(* A PASCAL - LANGUAGE IMPLEMENTATION OF SCAN DISK SCHEDULING ALGORITHM*)
PROGRAM SCANDiskScheduling(Input, Output);
uses dos;
Const
	MAX  = 199;
 	MIN  = 0;
 	SIZE = 50;
Type 
	Disk = ARRAY[1..SIZE + 1] OF INTEGER;
Var
	n, h, count, i  : integer;
    A, ST, T 	: Disk; 
  	AST		: Real;
   hours, minutes, seconds, milliseconds: word;
   seconds_count : longint;
   c_hours, c_minutes, c_seconds, c_milliseconds: word;

PROCEDURE sort(var a : Disk; n : integer);
Var
      i, j,tmp : Integer;
Begin	  
     for i := 1 to n-1 do
     Begin
           for j := (i + 1) to n do
           begin
                 if(a[i] > a[j])then 
				 begin
				 tmp := a[i]; a[i] := a[j]; a[j] := tmp; 
				 end
           end
     end      
end;
	
Procedure SCAN(a : Disk; st : Disk; var t : Disk; N : Integer; h : integer; var cnt : integer; var ast : real);
Var    
	i, J, L,  R, Temp : integer;
	LE, RE : Disk;
Begin
	 J := 1; L := 1; R := 1;
     for i := 1 to N do
     Begin
           if(a[i] > h)Then Begin RE[R] := a[i]; R := R + 1; End
           else
           begin LE[L] := a[i]; L := L + 1; end
     End;
     sort(LE, L-1);
     sort(RE, R-1);
     Temp := h;
     For i := (L - 1) DownTo 1 Do
     Begin
           t[J] := LE[i]; 
           cnt := cnt + abs(Temp - t[J]);
           st[J] := cnt;  Temp := t[J]; J := J + 1;
     End;      
     t[J] := MIN; 
     cnt := cnt + abs(Temp - t[J]);
      st[J] := cnt;  Temp := t[J]; J := J + 1;      
     for i := 1 To (R - 1) Do
     Begin
           t[J] := RE[i]; 
           cnt := cnt + abs(Temp - t[J]);
           st[J] := cnt;  Temp := t[J]; J := J + 1;
     ENd;
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
  writeln('***********SCAN DISK SCHEDULING ALGORITHM IN PASCAL *****************');
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
  (*perform SCAN on the Cylinder numbers or track locations *)
  SCAN(A, ST, T, n, h, count, AST);
  Writeln('The original list is: '); display(A, n);
  Writeln('The New(SCAN) list is: '); display(T, n+1);
  Writeln('The total head movement/seek time is: ', count);
  Writeln('The Average head movement/seek time is: ', AST:6:2);
  GetTime(c_hours, c_minutes, c_seconds, c_milliseconds);
  seconds_count := c_seconds - seconds + (c_minutes - minutes) * 60 + (c_hours - hours) * 3600;
  Writeln('The Execution time is: ',seconds_count:4,'seconds.');
  Writeln('Press any key to continue');	
  readln;
End.


