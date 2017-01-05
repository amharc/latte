#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void _function_8_printInt(int i) {
    printf("%d\n", i);
}

void _function_11_printString(char *str) {
    puts(str);
}

char *concat(char *lhs, char *rhs) {
    char *res = malloc(strlen(lhs) + strlen(rhs) + 1);
    strcpy(res, lhs);
    strcat(res, rhs);
    return res;
}

int _function_7_readInt(void) {
    int i;
    scanf("%d", &i);
    return i;
}

char *_function_10_readString(void) {
    char *ret;
    size_t n = 0;

    getline(&ret, &n, stdin);

    ret[strlen(ret) - 1] = 0;
    return ret;
}

void* alloc(unsigned sz) {
    return malloc(sz);
}

void* clone_object(void *data, size_t size) {
    void *res = malloc(size);
    memcpy(res, data, size);
    return res;
}

extern int _function_4_main();

int main() {
    return _function_4_main();
}
