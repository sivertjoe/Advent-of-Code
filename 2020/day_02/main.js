const fs = require('fs');

const allFileContents = fs.readFileSync('input', 'utf-8');
let content = allFileContents.split(/\r?\n/);
if(content[content.length - 1] === "")
{
    content.splice(-1);
}

function partOne() {
    let counter = 0;
    content.forEach(line => {
        const re = /(\d*)-(\d*) (\w): (\S+)/;
        const val = line.match(re);
    
        const n1 = parseInt(val[1]);
        const n2 = parseInt(val[2]);
        const ch = val[3];
        const s  = val[4];

        let occ = s.split(ch).length - 1;        
        counter += + (occ >= n1 && occ <= n2);
        
    });

    return counter;
}

function partTwo() {
    let counter = 0;
    content.forEach(line => {
        const re = /(\d*)-(\d*) (\w): (\S+)/;
        const val = line.match(re);
    
        const n1 = parseInt(val[1]) - 1;
        const n2 = parseInt(val[2]) - 1;
        const ch = val[3];
        const s  = val[4];
        
        const b1 = s[n1] == ch;
        const b2 = s[n2] == ch;
        
        
        // convert to number
        counter += + (b1 && !b2) || (!b1 && b2)
    });

    return counter;
}

function time(f) {
    const start = new Date()
    const ans = f()
    const end = new Date()

    const diff = end.getMilliseconds() - start.getMilliseconds()
    console.log(`(${diff}ms)\t${ans}`)
}

time(partOne)
time(partTwo)
