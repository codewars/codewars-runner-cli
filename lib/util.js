var fs = require('fs'),
    path = require('path'),
    config = require('./config');

// async control flow helper method: returns a function that will either call the exit function or the next function,
// depending on the presence of an error.
module.exports.then = function (exit, next) {
    return function (err) {
        if (err) {
            console.error(err);
            exit();
        }
        else {
            next();
        }
    };
};

module.exports.parseBodyToArgs = function (body) {
    return Object.keys(body).reduce(function (args, key) {
        var prefix = key.length < 3 ? '-' : '--';
        var value = body[key] || '';
        if (value) {
            value = value.replace(/"/g, '\\"');
            return args + prefix + key + ' "' + value + '" ';
        }
        else {
            return args;
        }
    }, '');
};

module.exports.parseBodyToArray = function (body, ignores) {
    var out = [];
    Object.keys(body).forEach(function (key) {
        if (!ignores || ignores.indexOf(key) == -1) {
            var prefix = key.length < 3 ? '-' : '--';
            var value = body[key] || '';

            out.push(prefix + key);
            if (value) out.push(value);
        }
    });

    return out;
};

// mkdir -p
function mkdirParentSync (dirPath, mode) {
    var dirs = dirPath.split("/"), partialPath;
    for (var idx = (dirPath[0] == '/' ? 1 : 0); idx < dirs.length; idx++) {
        partialPath = dirs.slice(0, idx + 1).join("/");
        if (!fs.existsSync(partialPath))
            fs.mkdirSync(partialPath, mode);
    }
}

// Infer the name of a file from a module or namespace declaration in the code
function codeFileName(language, code, defaultFileName) {
    if (config.moduleRegExs[language])
    {
        var match = config.moduleRegExs[language].exec(code);
        return match !== null ?
            match[1].replace(/\./g, '/').replace(/-/g, '_') + '.' + config.fileExtensions[language] :
            defaultFileName;
    }
    else
    {
        return defaultFileName || language
    }
}

module.exports.codeWriteSync = function (language, code, codeDir, defaultFileName, overwrite) {
    var fileName = codeFileName(language, code, defaultFileName);
    if (!(typeof fileName == 'string' || fileName instanceof String))
        throw new Error(["Could not determine valid name from code:\n\n", code].join(""));
    fileName = path.join(codeDir, fileName);
    if (fs.existsSync(fileName))
        if (!overwrite)
        {
            throw new Error(["Could not write code to file ", fileName,
                " because file already exists:\n\n", code].join(""));
        }
    mkdirParentSync(path.dirname(fileName));
    fs.writeFileSync(fileName, code);
    return fileName;
};