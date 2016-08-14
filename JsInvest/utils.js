var fs = require('fs'),
    Q = require("q"),
    //request = require("request").defaults({ proxy: "http://nn1003:Liu19875@sdcwsa01:80" });
    request = require("request");

exports.existsFile = function (file) {
    var deferred = Q.defer();
    fs.exists(file, function (result) {
        return result ? deferred.resolve(file) : deferred.reject('invalid file');
    });
    return deferred.promise;
}

exports.getUrl = function (url) {
    var deferred = Q.defer();

    request({ url: url }, function (error, response, body) {
        if (response && response.statusCode == 200)
            deferred.resolve(body);
        else
            deferred.reject(error);
    });

    return deferred.promise;
}
