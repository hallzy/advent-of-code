const { getInput, toInt } = require('../lib.js');

(
    async () =>
    {
        const input = await getInput();
        const data = input.group(toInt);
        const max = data.map(el => el.sum()).max(3);

        const part1 = max[0];
        const part2 = max.sum();

        console.log(`Part 1: ${part1}`);
        console.log(`Part 2: ${part2}`);
    }
)();
