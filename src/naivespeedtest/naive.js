inputdata = [];
total = 0;

for(let i = 0; i<1000000; i++){
    total = total + Math.random()*1000;
    input.push(parseInt(total);}

const smt = input => {
    const partition = [], result = []
    for (let i = 0; i < input.length; i++) partition[i] = input.slice(i, i + 8)
    partition.reduce((x, y) => {
        const s = y[y.length - 1] - y[0]
        if (s < 1000) result.push([y, s])
    }, result)
    return result}

const test = () => {
    start = performance.now();
    res = smt(inputdata);
    stop = performance.now();
    console.log(stop - start);
    return res;}

