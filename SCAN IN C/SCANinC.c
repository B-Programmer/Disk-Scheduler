/** A C - LANGUAGE IMPLEMENTATION OF SCAN DISK SCHEDULING ALGORITHM
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50
void SCAN(int a[], int st[], int t[], int N, int h, int *cnt, double *ast);
void sort(int a[], int n);
void display(int a[], int N);
int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE], T[SIZE+1];
  double AST;
  //start timming
  clock_t startTiming = clock();
  printf("*********** SCAN DISK SCHEDULING ALGORITHM IN C *****************");
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
  SCAN(A, ST, T, n, h, &count, &AST);
  printf("\nThe original SCAN list is: \n"); display(A, n);
  printf("\nThe New(SCAN) list is: \n"); display(T, n+1);
  printf("\nThe total head movement/seek time is: %d", count);
  printf("\nThe Average head movement/seek time is: %.2f\n", AST);
   //End timming
  clock_t EndTiming = clock();
  double ExecutionTime = (double)(EndTiming - startTiming)/CLOCKS_PER_SEC;
  printf("The Execution time is: %.2f seconds.\n", ExecutionTime);
  system("PAUSE");	
  return 0;
}
void SCAN(int a[], int st[], int t[], int N, int h, int *cnt, double *ast)
{    int i, J = 0, L = 0, R = 0, Temp, LE[N], RE[N];
     for(i = 0; i < N; i++)
     {
           if(a[i] > h) RE[R++] = a[i]; 
           else
           LE[L++] = a[i];
     }
     sort(LE, L);
     sort(RE, R);
     Temp = h;
     for(i = L-1; i >= 0; i--)
     {
           t[J] = LE[i]; 
           *cnt += (int)abs((double)Temp - t[J]);
           st[J] = *cnt;  Temp = t[J]; J++;
     }      
     t[J] = MIN; 
     *cnt += (int)abs((double)LE[0] - t[J]);
      st[J] = *cnt;  Temp = t[J]; J++;      
     for(i = 0; i < R; i++)
     {
           t[J] = RE[i]; 
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
