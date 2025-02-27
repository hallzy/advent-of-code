const fs = require('fs/promises');

// Get input from input.txt file
const getInput = async () =>
{
    const rawData = await fs.readFile('input.txt', { encoding: 'utf8' });
    return rawData.split("\n");
}

// Helper to convert a value to a base 10 integer
const toInt = x => parseInt(x, 10);

(
    async () =>
    {
        const input = await getInput()
        const data = input
            // Remove empty elements
            .filter(x => x !== '')
            .reduce
            (
                (acc, row) =>
                {
                    // Get each number on the row as separate elements of an
                    // array
                    const rowArr = row.split(/ +/).map(toInt);

                    acc.left.push(rowArr[0]);
                    acc.right.push(rowArr[1]);
                    return acc;
                },
                { left: [], right: [] }
            )
        ;

        const left = data.left.sort();
        const right = data.right.sort();

        let rightIdx = 0;

        const solutions = left.reduce
        (
            (acc, leftValue, leftIdx) =>
            {
                // For solution 1
                const rightValue1 = right[leftIdx];
                const diff = Math.abs(rightValue1 - leftValue);
                acc.sol1 += diff;

                // For Solution 2
                let matchesRight = 0;

                // Move through the numbers on the right until we are higher
                // than the number on the left
                while (right[rightIdx] <= leftValue)
                {
                    // Keep track of how many numbers on the right match the
                    // number on the left
                    if (right[rightIdx] === leftValue)
                    {
                        matchesRight++;
                    }
                    rightIdx++;
                }

                let matchesLeft = 0;
                // Now, how many numbers on the left match the number we were
                // looking at?
                while (left[leftIdx] === right[rightIdx - 1])
                {
                    matchesLeft++;
                    leftIdx++;
                }

                acc.sol2 += leftValue * matchesRight * matchesLeft;

                return acc;
            },
            { sol1: 0, sol2: 0 }
        );

        console.log(`Solution 1: ${solutions.sol1} ---- ${solutions.sol1 === 2031679 ? "Correct!" : "Wrong"}`);
        console.log(`Solution 2: ${solutions.sol2} ---- ${solutions.sol2 === 19678534 ? "Correct!" : "Wrong"}`);
    }
)();
