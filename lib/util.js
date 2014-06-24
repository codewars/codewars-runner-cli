var fs = require('fs');

// async control flow helper method: returns a function that will either call the exit function or the next function,
// depending on the presence of an error.
module.exports.then = function then(exit, next)
{
    return function (err)
    {
        if (err)
        {
            console.error(err);
            exit();
        }
        else
        {
            next();
        }
    };
}

// inline code gets saved to a file.
module.exports.writeSrc = function writeSrc(opts, key, cb)
{
    var file = 'tmp/' + key + '.' + opts.ext;

    // write the temporary file
    fs.writeFile(file, opts[key], function (err)
    {
        if (err)
        {
            cb(err);
        }
        else
        {
            opts[key + 'Src'] = file;
            cb();
        }
    });
};

module.exports.parseBodyToArgs = function parseBodyToArgs(body)
{
    return Object.keys(body).reduce(function (args, key)
    {
        var prefix = key.length < 3 ? '-' : '--';
        var value = body[key] || '';
        if (value)
        {
            value = value.replace(/"/g, '\\"');
            return args + prefix + key + ' "' + value + '" ';
        }
        else
        {
            return args;
        }
    }, '');
}

module.exports.parseBodyToArray = function parseBodyToArray(body, ignores)
{
    var out = [];
    Object.keys(body).forEach(function (key)
    {
        if (!ignores || ignores.indexOf(key) == -1)
        {
            var prefix = key.length < 3 ? '-' : '--';
            var value = body[key] || '';

            out.push(prefix + key);
            if (value) out.push(value);
        }
    });

    return out;
}


