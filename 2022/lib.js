const fs = require('fs/promises');

// Get input from input.txt file
exports.getInput = async () =>
{
    const rawData = await fs.readFile('input.txt', { encoding: 'utf8' });
    return rawData.split("\n");
}

// Helper to convert a value to a base 10 integer
exports.toInt = x => parseInt(x, 10);

// Returns the sum of the values in an array
Array.prototype.sum = function()
{
    return this.reduce((acc, val) => acc + val, 0);
};

// Returns the largest 'count' number of elements in an array
Array.prototype.max = function(count = 1)
{
    return [ ...this ].sort( (a, b) => b - a ).slice(0, count);
};

// Returns the smallest 'count' number of elements in an array
Array.prototype.min = function(count = 1)
{
    return [ ...this ].sort( (a, b) => a - b ).slice(0, count);
};

// Group elements of an array delimited by an empty string. Optionally, provide
// a mapper function that will be used to map each element of the initial array
// which isn't the empty string.
Array.prototype.group = function(mapper = x => x)
{
    let ret = [];
    let current = [];

    this.forEach
    (
        el =>
        {
            if (el === '')
            {
                ret.push(current);
                current = [];
            }
            else
            {
                current.push(mapper(el));
            }
        }
    );

    return ret;
};
