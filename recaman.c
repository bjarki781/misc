#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define ARR_SIZE 1000

int *intsrch(int *base, int key, size_t len)
{
    for (int i = 0; i < len; i++) {
            if (base[i] == key)
                    return &base[i];
    }
    return NULL;
}

int main()
{
    int n, *arr;

    arr = calloc(ARR_SIZE, sizeof(int));

    for (n=1; n<ARR_SIZE; n++) {
        if (intsrch(arr, arr[n-1]-n, n) == NULL && arr[n-1]-n > 0) {
            arr[n] = arr[n-1]-n;
        } else {
            arr[n] = arr[n-1]+n;
        }
    }

    for (n=0; n<ARR_SIZE; n++)
        printf("%i\n", arr[n]);
}

