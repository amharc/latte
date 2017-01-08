#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct string {
    size_t length;
    char str[];
};

struct string empty_string = { 0 };

#define STR(x) (((x) == NULL) ? &empty_string : (x))

void _function_8_printInt(int i) {
    printf("%d\n", i);
}

void _function_11_printString(struct string *_str) {
    struct string *str = STR(_str);
    fwrite(str->str, sizeof(char), str->length, stdout);
    putchar('\n');
}

struct string *concat(struct string *_lhs, struct string *_rhs) {
    struct string *lhs = STR(_lhs);
    struct string *rhs = STR(_rhs);

    struct string *res = malloc(sizeof(size_t) + lhs->length + rhs->length);
    res->length = lhs->length + rhs->length;
    memcpy(res->str, lhs->str, lhs->length);
    memcpy(res->str + lhs->length, rhs->str, rhs->length);
    return res;
}

int _function_7_readInt(void) {
    int i;
    scanf("%d", &i);
    return i;
}

struct string *_function_10_readString(void) {
    char *tmp;
    size_t n = 0;

    scanf(" ");
    n = getline(&tmp, &n, stdin);

    if (tmp[n - 1] == '\n') {
        --n;
    }

    struct string *str = malloc(sizeof(size_t) + n);
    str->length = n;
    memcpy(str->str, tmp, n);

    free(tmp);
    return str;
}

void* alloc(unsigned sz) {
    return calloc(sz, 1);
}

void* clone_object(void *data, size_t size) {
    void *res = malloc(size);
    memcpy(res, data, size);
    return res;
}

void _function_5_error(void) {
    fprintf(stderr, "error\n");
    exit(1);
}

extern int _function_4_main();

int main() {
    return _function_4_main();
}
