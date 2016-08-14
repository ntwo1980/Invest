var fs = require('fs'),
    readline = require("readline"),
    path = require("path"),
    _ = require("lodash"),
    jsdom = require('jsdom').jsdom,
    document = jsdom("<html></html>", {}),
    window = document.defaultView,
    $ = require('jquery')(window),
    Q = require("q"),
    moment = require("moment"),
    utils = require("./utils"),
    home = process.env.HOME || process.env.USERPROFILE,
    download = home.replace(/\\/g, "/") + "/" + "Downloads",
    rPath = path.resolve(__dirname, "../RInvest/sw"),
    csvhead = "Code,Name,Date,Open,High,Low,Close,Volumn,Amount,Change,Turnover,PE,PB,Average,AmountPercentage,HQLTSZ,AHQLTSZ,Payout",
    variableCount = csvhead.split(",").length;

function processDataFile(file) {
    return Q.promise(function (resolve, reject) {
        var xlsPath = path.resolve(download, file),
            csvName = file.substr(0, file.indexOf("_")) + ".csv",
            csvPath = path.resolve(rPath, csvName),
            data,
            lastLocalDataDateStr = "1990-01-01";

        utils.existsFile(csvPath)
            .fail(function () {
                return Q.nfcall(fs.appendFile, csvPath, csvhead);
            })
            .then(function () {
                return Q.nfcall(fs.readFile, xlsPath, "utf-8");
            })
            .then(function (fData) {
                data = fData;
                var deferred = Q.defer(),
                    lineNo = 0;

                var rd = readline.createInterface({
                    input: fs.createReadStream(csvPath),
                    output: process.stdout,
                    terminal: false
                });

                rd.on("line", function (line) {
                    if (lineNo++ > 1) {
                        var cells = line.split(",");

                        lastLocalDataDateStr = cells[2];
                    }
                });

                rd.on("close", function () {
                    deferred.resolve();
                });

                return deferred.promise;
            })
            .then(function () {
                console.log("processing " + file);
                lastLocalDataDate = moment(lastLocalDataDateStr);

                var table = $(data),
                    trs = table.find("tr"),
                    lines = [], line = [];

                var encounterOldData = false;
                _.forEach(trs, function (tr, trIndex) {
                    if (encounterOldData)
                        return false;

                    _.forEach($(tr).find("td"), function (td, tdIndex) {
                        var text = $(td).text().replace(",", "");

                        if (tdIndex == 2) {
                            text = text.substr(0, text.indexOf(" "));  // remove time

                            lastRemoteDataDate = moment(text);
                            var dateDiff = lastRemoteDataDate.diff(lastLocalDataDate, 'days');
                            if (dateDiff <= 0) {
                                encounterOldData = true;
                                line = [];
                                return false;
                            }
                        }

                        line.push(text);
                    }); // end tr

                    if (line.length) {
                        var missingVariableCount = variableCount - line.length;

                        lines.push(line.join(",") + _.repeat(",", missingVariableCount > 0 ? missingVariableCount : 0));
                        line = [];
                    }
                }); // end trs

                if (lines.length > 0) {
                    fs.appendFileSync(csvPath, "\r\n" + _.reverse(lines).join("\r\n"), "utf-8");
                    lines = [];
                    line = [];
                }
            })
            .then(function () {
                resolve(file);
            });
    });
}

exports.getData = function () {
    Q.nfcall(fs.readdir, download)
        .then(function (files) {
            return _.filter(files, function (file) {
                return _.endsWith(file, "_hq.xls");
            });
        })
        .then(function (dataFiles) {
            var t = _.reduce(dataFiles, function (promises, file) {
                var promise = processDataFile(file);

                promises.push(promise);

                return promises;
            }, []);

            return t;
        })
        .then(function (promises) {
            return Q.allSettled(promises);
        })
        .then(function (results) {
            var fails = _.filter(results, function (result) { return result.state != "fulfilled" });

            if (!fails.length)
                console.log("done");
            else
                _.forEach(fails, function (fail) {
                    console.error("Failed: " + fail.reason);
                });
        });
};


