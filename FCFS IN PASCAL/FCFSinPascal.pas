(* A PASCAL - LANGUAGE IMPLEMENTATION OF FCFS DISK SCHEDULING ALGORITHM*)
PROGRAM FCFSDiskScheduling(input, output);
uses dos;
Const
 	MAX  = 199;
 	MIN  = 0;
 	SIZE = 50;

Type 
	Disk = ARRAY[1..SIZE] OF INTEGER;
Var
	n, h, count, i  : integer;
    A, ST, T 	: Disk;
  	AST		: Real;
    hours, minutes, seconds, milliseconds: word;
    seconds_count : longint;
    c_hours, c_minutes, c_seconds, c_milliseconds: word;

PROCEDURE  FCFS(a : Disk; st : Disk; var t : Disk; N : integer; h : integer; var cnt : integer; var ast : real);
VAR    
 	i : integer; 
begin
     cnt := cnt + abs(h - a[1]);
     st[1] := cnt;  t[1] := a[1];
     for i := 2 to N do
     begin
      cnt := cnt + abs(a[i] - a[i-1]);
      st[i] := cnt;  t[i] := a[i];
     end;
     ast := cnt/N;
     
end;
PROCEDURE display(a : Disk; N : integer);
VAR
     i : integer;
begin
    for i := 1 to N do
    Write(a[i]: 4);
    Writeln;	
end;

Begin
  GetTime(hours, minutes, seconds, milliseconds);
  writeln('***********FCFS DISK SCHEDULING ALGORITHM IN PASCAL *****************');
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
  (*perform FCFS on the Cylinder numbers or track locations *)
  FCFS(A, ST, T, n, h, count, AST);
  Writeln('The original list is: '); display(A, n);
  Writeln('The New(FCFS) list is: '); display(T, n);
  Writeln('The total head movement/seek time is: ', count);
  Writeln('The Average head movement/seek time is: ', AST:6:2);
  GetTime(c_hours, c_minutes, c_seconds, c_milliseconds);
  seconds_count := c_seconds - seconds + (c_minutes - minutes) * 60 + (c_hours - hours) * 3600;
  Writeln('The Execution time is: ',seconds_count:4,'seconds.');
  Writeln('Press any key to continue');	
  readln;
End.
