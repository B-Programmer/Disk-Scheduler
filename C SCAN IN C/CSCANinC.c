/** A C - LANGUAGE IMPLEMENTATION OF C-SCAN DISK SCHEDULING ALGORITHM
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50
void CSCAN(int a[], int st[], int t[], int N, int h, int *cnt, double *ast);
void sort(int a[], int n);
void display(int a[], int N);
int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE+2], T[SIZE+2];
  double AST;
  //start timming
  clock_t startTiming = clock();
  printf("*********** C-SCAN DISK SCHEDULING ALGORITHM IN C *****************");
  do {
  printf("\nRead in the total number of cylinders between 1 and 50: N\n");
  scanf("%d",&n);
  }while(n < 1 || n > SIZE);
  printf("\nRead in %d disk cylinder numbers or track locations", n);
  for(i = 0; i < n; i++)
  {
   do
   { printf("\nRead in disk cylinder number or track location %d, between 0 and 199: ", i);      
     scanf("%d", &A[i]);  
   }while((A[i] < MIN) || (A[i] > MAX));
  }
   do
   { printf("\nRead in the initial disk head position between 0 and 199: ");      
     scanf("%d", &h);  
   }while((h < MIN) || (h > MAX));
  //perform SCAN on the Cylinder numbers or track locations
  CSCAN(A, ST, T, n, h, &count, &AST);
  printf("\nThe original list is: \n"); display(A, n);
  printf("\nThe New(C-SCAN) list is: \n"); display(T, n+2);
  printf("\nThe total head movement/seek time is: %d", count);
  printf("\nThe Average head movement/seek time is: %.2f\n", AST);
   //End timming
  clock_t EndTiming = clock();
  double ExecutionTime = (double)(EndTiming - startTiming)/CLOCKS_PER_SEC;
  printf("The Execution time is: %.2f seconds.\n", ExecutionTime);
  system("PAUSE");	
  return 0;
}
void CSCAN(int a[], int st[], int t[], int N, int h, int *cnt, double *ast)
{    int i, J = 0, L = 0, R = 0, Temp, Temp1, LE[N], RE[N];
     for(i = 0; i < N; i++)
     {
           if(a[i] > h) RE[R++] = a[i]; 
           else
           LE[L++] = a[i];
     }
     sort(LE, L);
     sort(RE, R);
     Temp = h;
     for(i = 0; i < R; i++)
     {
           t[J] = RE[i]; 
           *cnt += (int)abs((double)Temp - t[J]);
           st[J] = *cnt;  Temp = t[J]; J++;
     }
     Temp = MAX; Temp1 = RE[R-1];
     for(i = 0; i <= 1; i++)
     {
           t[J] = Temp; 
           *cnt += (int)abs((double)Temp1 - Temp);
           st[J] = *cnt;  Temp1 = Temp; Temp = MIN; J++;      
     }
     for(i = 0; i < L; i++)
     {
           t[J] = LE[i]; 
           *cnt += (int)abs((double)Temp - t[J]);
           st[J] = *cnt;  Temp = t[J]; J++;
     }           
     *ast = (double)*cnt/N;
     
}
void sort(int a[], int n)
{
     int i, j,tmp;
     for(i = 0; i < n-1; i++)
     {
           for(j = i+1; j < n; j++)
           {
                 if(a[i] > a[j]){ tmp = a[i]; a[i] = a[j]; a[j] = tmp; }
           }
     }      
}
void display(int a[], int N)
{
    int i;
    for(i = 0; i < N; i++)
    printf("%d ", a[i]);
}
