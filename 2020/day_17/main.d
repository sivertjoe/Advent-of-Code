import std.algorithm, std.stdio, std.conv, std.array;

alias Layers = char[][][];
alias Box = char[][][][];

int count(Layers layers, int N)
{
    int sum = 0;
    for(int i = 0; i < N; i++)
    {
        for(int j = 0; j < N; j++)
        {
            for(int k = 0; k < N; k++)
            {
                if(layers[i][j][k] == '#')
                {
                    sum += 1;
                }
            }
        }
    }

    return sum;
}

Layers empty(int N)
{
    auto layers = new char[][][N];

    for(int i = 0; i < N; i++)
    {
        layers[i] = new char[][N];
        for(int j = 0; j < N; j++)
        {
            layers[i][j] = new char[N];
            for(int k = 0; k < N; k++)
            {
                layers[i][j][k] = '.';
            }
        }
    }
    return layers;
}

char get_ijk(Layers curr, int i, int j, int k, int N)
{
    if(i < 0 || j < 0 || k < 0 || i >= N || j >= N || k >= N)
    {
        return '.';
    }

    return curr[i][j][k];
}

char check_neighbors(Layers curr, int N, int i, int j, int k)
{
    int sum = 0;
    char me = curr[i][j][k];

    for(int x = -1; x < 2; x++)
    {
        for(int y = -1; y < 2; y++)
        {
            for(int z = -1; z < 2; z++)
            {
                if(!(x == 0 && y == 0 && z == 0))
                {
                    int xx = i + x;
                    int yy = j + y;
                    int zz = k + z;
                    if(get_ijk(curr, xx, yy, zz, N) == '#')
                    {
                        sum += 1;
                    }
                }
            }
        }
    }

    if(me == '#' && (sum == 2 || sum == 3))
    {
        return '#';
    }
    else if(me == '.' && sum == 3)
    {
        return '#';
    }

    return '.';
}

Layers next(Layers curr, int N)
{
    auto next = empty(N);

    for(int i = 0; i < N; i++)
    {
        for(int j = 0; j < N; j++)
        {
            for(int k = 0; k < N; k++)
            {
                next[i][j][k] = check_neighbors(curr, N, i, j, k);
            }
        }
    }
    return next;
}

int partOne(string[] slice)
{
    int N = 20;
    auto layers = empty(N);

    {
        auto mid = N / 2;
        auto start = mid - slice.length / 2;

        auto i = start;
        foreach(line; slice)
        {
            auto j = start;
            foreach(ch; line)
            {
                layers[mid][i][j] = ch;
                j++;
            }
            i++;
        }
    }

    for(int i = 0; i < 6; i++)
    {
        layers = next(layers, N);
    }

    return count(layers, N);
}

int count2(Box box, int N)
{
    int sum = 0;
    for(int w = 0; w < N; w++)
    {
        for(int i = 0; i < N; i++)
        {
            for(int j = 0; j < N; j++)
            {
                for(int k = 0; k < N; k++)
                {
                    if(box[w][i][j][k] == '#')
                    {
                        sum += 1;
                    }
                }
            }
        }
    }

    return sum;
}

char get_wijk(Box box, int w, int i, int j, int k, int N)
{
    if(w < 0 || i < 0 || j < 0 || k < 0 || w >= N || i >= N || j >= N || k >= N)
    {
        return '.';
    }

    return box[w][i][j][k];
}

char check_neighbors2(Box curr, int N, int w, int i, int j, int k)
{
    int sum = 0;
    char me = curr[w][i][j][k];

    for (int q = -1; q < 2; q++)
    {
        for(int x = -1; x < 2; x++)
        {
            for(int y = -1; y < 2; y++)
            {
                for(int z = -1; z < 2; z++)
                {
                    if(!(q == 0 && x == 0 && y == 0 && z == 0))
                    {
                        int qq = w + q;
                        int xx = i + x;
                        int yy = j + y;
                        int zz = k + z;
                        if(get_wijk(curr, qq, xx, yy, zz, N) == '#')
                        {
                            sum += 1;
                        }
                    }
                }
            }
        }
    }

    if(me == '#' && (sum == 2 || sum == 3))
    {
        return '#';
    }
    else if(me == '.' && sum == 3)
    {
        return '#';
    }

    return '.';
}

Box next2(Box curr, int N)
{
    auto next = empty2(N);

    for(int w = 0; w < N; w++)
    {
        for(int i = 0; i < N; i++)
        {
            for(int j = 0; j < N; j++)
            {
                for(int k = 0; k < N; k++)
                {
                    next[w][i][j][k] = check_neighbors2(curr, N, w, i, j, k);
                }
            }
        }
    }
    return next;
}
Box empty2(int N)
{
    Box box = new char[][][][N];
    for(int w = 0; w < N; w++)
    {
        box[w] = new char[][][N];
        for(int i = 0; i < N; i++)
        {
            box[w][i] = new char[][N];
            for(int j = 0; j < N; j++)
            {
                box[w][i][j] = new char[N];
                for(int k = 0; k < N; k++)
                {
                    box[w][i][j][k] = '.';
                }
            }
        }
    }
    return box;
}

int partTwo(string[] slice)
{
    int N = 20;
    auto box = empty2(N);
    {
        auto mid = N / 2;
        auto start = mid - slice.length / 2;

        auto i = start;
        foreach(line; slice)
        {
            auto j = start;
            foreach(ch; line)
            {
                box[mid][mid][i][j] = ch;
                j++;
            }
            i++;
        }
    }

    for(int i = 0; i < 6; i++)
    {
        box = next2(box, N);
    }

    return count2(box, N);
}

void main()
{
    auto input = File("input")
                    .byLine()
                    .map!(s => to!string((s)))
                    .array();

    writeln(partOne(input));
    writeln(partTwo(input));
}
