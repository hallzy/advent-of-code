const { getInput, toInt } = require('../lib.js');

// A = Rock
// B = Paper
// C = Scissors

// ---- Part 1
// X = Rock
// Y = Paper
// Z = Scissors

// ---- Part 2
// X = Need to Lose
// Y = Need to Draw
// Z = Need to Win

const choiceScore1 = { X: 1, Y: 2, Z: 3 };
const result1 = {
    A: { X: 3, Y: 6, Z: 0 },
    B: { X: 0, Y: 3, Z: 6 },
    C: { X: 6, Y: 0, Z: 3 },
};


const choiceScore2 = { X: 0, Y: 3, Z: 6 };
const result2= {
    A: { X: 'Z', Y: 'X', Z: 'Y' },
    B: { X: 'X', Y: 'Y', Z: 'Z' },
    C: { X: 'Y', Y: 'Z', Z: 'X' },
};

(
    async () =>
    {
        const input = (await getInput()).filter(v => v);

        let part1 = 0;
        let part2 = 0;

        input.forEach
        (
            plays =>
            {
                const opponent = plays[0];

                const me1 = plays[2];

                part1 += choiceScore1[me1] + result1[opponent][me1];

                const expectedResult2 = plays[2];
                const me2 = result2[opponent][expectedResult2];

                part2 += choiceScore1[me2] + choiceScore2[expectedResult2];
            }
        );

        console.log(`Part 1: ${part1}`);
        console.log(`Part 2: ${part2}`);
    }
)();
