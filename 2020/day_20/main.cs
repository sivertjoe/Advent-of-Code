using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Collections.Generic;

namespace Program {
class Day20
{
    class Tile
    {
        public Tile(string tileInfo)
        {
            list = new List<string>();
            string[] lines = tileInfo.Split("\n");

            var sid = lines [0]
                        .Split(" ")[1];
            sid = sid.Remove(sid.Length - 1);

            id = ulong.Parse(sid);

            foreach (var line in lines[1..]) {
                if (!String.IsNullOrEmpty(line)) {
                    list.Add(line);
                }
            }
        }

        public void RemoveBoarder()
        {
            var newList = new List<string>();

            list.RemoveAt(list.Count - 1);
            list.RemoveAt(0);

            foreach (var str in list) {
                newList.Add(str.Substring(1, str.Length - 2));
            }

            list = newList;
        }

        public Tile()
        {
            list = new List<string>();
            id = 0;
        }

        public void Flip(int n)
        {
            for (int i = 0; i < n; i++) {
                this.RotateRight();
            }

            var newList = new List<string>();
            for (int i = list.Count() - 1; i >= 0; i--) {
                newList.Add(list[i]);
            }
            list = newList;
        }

        public void RotateRight()
        {
            var newList = new List<string>();
            for (int i = 0; i < 10; i++) {
                var temp = new StringBuilder();
                for (int j = 0; j < 10; j++) {
                    temp.Append(list [10 - 1 - j]
                                [i]);
                }
                newList.Add(temp.ToString());
            }
            list = newList;
        }

        public ulong id;
        public List<string> list;
    }

    static List<Tile> readInput()
    {
        var res = new List<Tile>();

        var text = File.ReadAllText("input");

        var tiles = text.Split("\n\n");
        foreach (var tile in tiles) {
            if (tile != "") {
                res.Add(new Tile(tile));
            }
        }

        return res;
    }

    static string getSide(Tile tile, int side)
    {
        switch (side) {
            // top
            case 0:
                return tile.list[0];

            // right
            case 1: {
                var res = new StringBuilder();
                var n = tile.list[0].Length - 1;

                for (int i = 0; i < tile.list.Count(); i++) {
                    res.Append(tile.list [i]
                               [n]);
                }
                return res.ToString();
            }

            // bottom
            case 2: {
                var n = tile.list[0].Length - 1;
                return tile.list[n];
            }

            // left
            case 3: {
                var res = new StringBuilder();
                for (int i = 0; i < tile.list.Count(); i++) {
                    res.Append(tile.list [i]
                               [0]);
                }
                return res.ToString();
            }

            default:
                throw new Exception("unreachable");
        }
    }

    static bool checkSide(Tile a, Tile b, int side)
    {
        var s = getSide(b, side);
        for (int j = 0; j < 4; j++) {
            if (s == getSide(a, j)) {
                return true;
            }
        }
        return false;
    }

    static bool tryFit(Tile a, Tile b, int side)
    {
        var s = getSide(b, side);
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                if (s == getSide(a, side)) {
                    return true;
                }
                a.RotateRight();
            }
            a.Flip(i);
        }
        return false;
    }

    static Tile findSomeCorner(List<Tile> tiles)
    {
        for (int i = 0; i < tiles.Count(); i++) {
            var n = 0;
            for (int j = 0; j < tiles.Count(); j++) {
                if (i == j)
                    continue;

                for (int side = 0; side < 4; side++) {
                    if (tryFit(tiles[i], tiles[j], side)) {
                        n++;
                    }
                }
            }
            // Found a corner
            if (n == 2) {
                return tiles[i];
            }
        }

        throw new Exception("unreachable");
    }

    // Find the TOP LEFT corner and rotate it properly
    static Tile findCorner(List<Tile> tiles)
    {
        Tile corner = findSomeCorner(tiles);
        const int LEFT = 3;
        const int TOP = 0;

        for (int m = 0; m < 4; m++) {
            for (int n = 0; n < 4; n++) {
                var correctCorner = !tiles.Exists(t => t.id == corner.id ? false
                                                  : tryFit(t, corner, LEFT) ||
                                                      tryFit(t, corner, TOP));

                if (correctCorner) {
                    return corner;
                }
                corner.RotateRight();
            }
            corner.Flip(m);
        }

        throw new Exception("unreachable");
    }

    static bool tryFit(Tile a, int side1, Tile b, int side2)
    {
        var s = getSide(b, side2);
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                if (s == getSide(a, side1)) {
                    return true;
                }
                a.RotateRight();
            }
            a.Flip(i);
        }
        return false;
    }

    static Tile findFitting(List<Tile> tiles, Tile tile, int side1, int side2)
    {
        for (int i = 0; i < tiles.Count(); i++) {
            if (tryFit(tiles[i], side2, tile, side1)) {
                return tiles[i];
            }
        }
        throw new Exception("unreachable!");
    }

    static Tile[, ] createTiles(List<Tile> tiles)
    {
        int N = tiles.Count() == 144 ? 12 : 3;
        Tile[, ] array = new Tile[N, N];

        Tile t = findCorner(tiles);
        array[0, 0] = t;
        tiles.Remove(t);

        const int TOP = 0;
        const int BOTTOM = 2;
        const int RIGHT = 1;
        const int LEFT = 3;

        for (int i = 0; i < N; i++) {
            if (i != 0) {
                var elem = findFitting(tiles, array[i - 1, 0], BOTTOM, TOP);
                array[i, 0] = elem;
                tiles.Remove(elem);
            }

            for (int j = 1; j < N; j++) {
                var elem = findFitting(tiles, array[i, j - 1], RIGHT, LEFT);
                array[i, j] = elem;
                tiles.Remove(elem);
            }
        }

        return array;
    }

    static ulong partOne(Tile[, ] mat)
    {
        int N = mat.GetLength(0);

        return mat[0, 0].id * mat[0, N - 1].id * mat[N - 1, 0].id *
               mat[N - 1, N - 1].id;
    }

    struct Point
    {
        public int x;
        public int y;
    }

    static List<Point> getMonsterPattern()
    {
        var pattern = "                  # \n" 
                    + "#    ##    ##    ###\n" 
                    + " #  #  #  #  #  #   ";

        int x = 0;
        int y = 0;
        var res = new List<Point>();
        foreach (var s in pattern) {
            if (s == '#') {
                res.Add(new Point(){ x = x, y = y });
            }

            if (s == '\n') {
                y++;
                x = 0;
            } else {
                x++;
            }
        }

        return res;
    }

    static int countMonsters(Tile tile, List<Point> mp)
    {
        var list = tile.list;

        (int maxy, int maxx) = mp.Aggregate(
          (0, 0),
          (acc, p) =>(Math.Max(acc.Item1, p.x), Math.Max(acc.Item2, p.y)));

        int N = list.Count;

        int sum = 0;
        for (int i = 0; i < N - maxx; i++) {
            for (int j = 0; j < N - maxy; j++) {
                var allEquals = !mp.Exists(point => list [i + point.y]
                                                    [j + point.x] != '#');
                sum += allEquals ? 1 : 0;
            }
        }
        return sum;
    }

    static int count(Tile tile) => tile.list.Aggregate(
      0,
      (acc, line) => acc +
                     line.Aggregate(0, (acc, ch) => acc + (ch == '#' ? 1 : 0)));

    static int partTwo(Tile[, ] mat)
    {
        int N = mat.GetLength(0);

        var list = new List<string>();
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                mat [i, j]
                  .RemoveBoarder();
            }
        }

        int t_length = mat[0, 0].list[0].Length;
        for (int i = 0; i < N; i++) {
            for (int k = 0; k < t_length; k++) {
                var builder = new StringBuilder();
                for (int j = 0; j < N; j++) {
                    builder.Append(mat[i, j].list[k]);
                }
                list.Add(builder.ToString());
                builder.Clear();
            }
        }

        var tile = new Tile();
        tile.list = list;

        var mp = getMonsterPattern();

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                var sum = countMonsters(tile, mp);
                if (sum > 0) {
                    return count(tile) - sum * mp.Count;
                }
                tile.RotateRight();
            }
            tile.Flip(i);
        }

        return -1;
    }

    static void Main(string[] args)
    {
        var tiles = readInput();
        var mat = createTiles(tiles);
        System.Console.WriteLine($"{partOne(mat)}");
        System.Console.WriteLine($"{partTwo(mat)}");
    }
}
}
