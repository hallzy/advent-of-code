const fs = require('fs/promises');

// Get input from input.txt file
const getInput = async () =>
{
    const rawData = await fs.readFile('input.txt', { encoding: 'utf8' });
    return rawData.split("\n");
}

const isDesc = (a, b) => b < a;
const isAsc = (a, b) => b > a;
const withinBounds = (a, b) => {
    let diff = Math.abs(a - b);
    return diff >= 1 && diff <= 3;
};

Array.prototype.sum = function() { return this.reduce((acc, v) => acc += Number(v), 0) };

Array.prototype.isSafe = function(toSkip = []) {
    let dir = null;
    let lastNum = null;

    return this.every
    (
        (num, i) =>
        {
            if (toSkip.indexOf(i) !== -1)
            {
                // Skip this value;
                return true;
            }

            if (lastNum === null)
            {
                lastNum = num;
                return true;
            }

            if (dir === null)
            {
                dir = isDesc(lastNum, num) ? isDesc : isAsc;
            }

            let res = dir(lastNum, num) && withinBounds(lastNum, num);
            lastNum = num;
            return res;
        }
    );
};

// Helper to convert a value to a base 10 integer
const toInt = x => parseInt(x, 10);

(
    async () =>
    {
        const input = await getInput()
        const solutions = input
            .filter(x => x !== '')
            .reduce
            (
                (acc, row) =>
                {
                    const rowArr = row.split(/ +/).map(toInt);

                    const res1 = rowArr.isSafe();
                    let res2 = res1;

                    if (!res2)
                    {
                        for (const idx of rowArr.keys()) {
                            res2 = rowArr.isSafe([idx]);
                            if (res2)
                            {
                                break;
                            }
                        }
                    }

                    acc.sol1 += Number(res1);
                    acc.sol2 += Number(res2);
                    return acc;
                },
                { sol1: 0, sol2: 0 }
            )
        ;

        console.log(`Solution 1: ${solutions.sol1} ---- ${solutions.sol1 === 479 ? "Correct!" : "Wrong"}`);
        console.log(`Solution 2: ${solutions.sol2} ---- ${solutions.sol2 === 531 ? "Correct!" : "Wrong"}`);
    }
)();
