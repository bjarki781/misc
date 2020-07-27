#include<stdio.h>

void printn(int, int);

int main(char *argv[])
{
    printn(45, 4);
    putchar('\n');
    return 0;
}

void printn(int n, int b)
{
    auto int a;
    if (a = n/b) {
            printn(a,b);
    }
    putchar(n % b + '0');

}
