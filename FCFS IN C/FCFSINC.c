/** A C - LANGUAGE IMPLEMENTATION OF FCFS DISK SCHEDULING ALGORITHM
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50
void FCFS(int a[], int st[], int t[], int N, int h, int *cnt, double *ast);
void display(int a[], int N);
int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE], T[SIZE];
  double AST;
  //start timming
  clock_t startTiming = clock();
  printf("***********FCFS DISK SCHEDULING ALGORITHM IN C *****************");
  do {
  printf("\nRead in the total number of cylinders between 1 and 50: N\n");
  scanf("%d",&n);
  }while(n < 1 || n > 50);
  printf("\nRead in %d disk cylinder numbers or track locations", n);
  for(i = 0; i < n; i++)
  {
   do
   { printf("\nRead in disk cylinder number or track location %d: between 0 and 199 ", i);      
     scanf("%d", &A[i]);  
   }while((A[i] < MIN) || (A[i] > MAX));
  }
   do
   { printf("\nRead in the initial disk head position between 0 and 199 ");      
     scanf("%d", &h);  
   }while((h < MIN) || (h > MAX));
  //perform FCFS on the Cylinder numbers or track locations
  FCFS(A, ST, T, n, h, &count, &AST);
  printf("\nThe original list is: \n"); display(A, n);
  printf("\nThe New(FCFS) list is: \n"); display(T, n);
  printf("\nThe total head movement/seek time is: %d", count);
  printf("\nThe Average head movement/seek time is: %.2f\n", AST);
  //End timming
  clock_t EndTiming = clock();
  double ExecutionTime = (double)(EndTiming - startTiming)/CLOCKS_PER_SEC;
  printf("The Execution time is: %.2f seconds.\n", ExecutionTime);
  system("PAUSE");	
  return 0;
}
void FCFS(int a[], int st[], int t[], int N, int h, int *cnt, double * ast)
{    int i;
     *cnt += (int)abs((double)h - a[0]);
     st[0] = *cnt;  t[0] = a[0];
     for(i = 1; i < N; i++)
     {
      *cnt += (int)abs((double)a[i] - a[i-1]);
      st[i] = *cnt;  t[i] = a[i];     
     }
     *ast = (double)*cnt/N;
     
}
void display(int a[], int N)
{
    int i;
    for(i = 0; i < N; i++)
    printf("%d ", a[i]);
}
