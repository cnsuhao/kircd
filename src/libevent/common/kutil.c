
// http://www.cse.yorku.ca/~oz/hash.html
unsigned int kutil_hash(const unsigned char *str)
{
    unsigned int hash = 5381;
    int c;

    c = *str;
    while (c)
    {
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

        str++;
        c = *str;
    }

    return hash;
}

