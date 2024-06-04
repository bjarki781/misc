int printf(const char *restrict format, ...); 
void *incr_quot(char *p, char *q)
{
    int i = 0, j = 0;
    char c;
    while ((c = p[i++])) {
        if (c == '"' || c == '\\')
            q[j++] = '\\';
        q[j++] = c;
    }
    q[j] = '\x00';
}

int main() 
{ 
    char q[300]; 
    char *p="int printf(const char *restrict format, ...); void *incr_quot(char *p, char *q) { int i = 0, j = 0; char c; while ((c = p[i++])) { if (c == \'\"\' || c == \'\\\\\') q[j++] = \'\\\\\'; q[j++] = c; } q[j] = \'\\x00\'; } int main(){char q[300]; char *p=\"%s\"; incr_quot(p,q); printf(p,q);return 0;}"; 
    incr_quot(p, q); 
    printf(p, q); 
    return 0;
}

