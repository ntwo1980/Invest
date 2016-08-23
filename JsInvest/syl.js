var fs = require('fs'),
    readline = require("readline"),
    path = require("path"),
    _ = require("lodash"),
    jsdom = require('jsdom').jsdom,
    document = jsdom("<html></html>", {}),
    window = document.defaultView,
    $ = require('jquery')(window),
    Q = require("q"),
    qlimit = require("qlimit"),
    limit = qlimit(10),
    moment = require("moment"),
    utils = require("./utils"),
    home = process.env.HOME || process.env.USERPROFILE,
    rPath = path.resolve(__dirname, "../RInvest/zz"),
    csvhead = "Code,Name,Date,PE,DPE,PB",
    variableCount = csvhead.split(",").length;


exports.getData = function () {
    var codes = [
        "000001",
        "002142",
        "600000",
        "600015",
        "600016",
        "600036",
        "601009",
        "601166",
        "601169",
        "601288",
        "601328",
        "601398",
        "601818",
        "601939",
        "601988",
        "601998"
    ];

    var commands = _.map(codes, limit(function (code) {
        return utils.postUrl("http://www.51shiyinglv.com/stockservice.svc/GetPEByStockCode",
            { "Content-Type": "application/json" },
            { "stockCode": code })
            .then(function (body) {
                return body;
            }, function (eror) { });
    }));

    Q.all(commands).done(function (data) {
        var data = _.reject(data, function (item) {
            return !item;
        }), stocks = {};

        if (!data.length)
            return;

        _.forEach(data, function (body) {
            processStock(body);
        });
    });

    function processStock(data) {
        var data = data.d.Result,
            code = data.StockCode,
            name = data.StockName,
            dates = data.PEValues.categories,
            pe = data.PEValues.data,
            dpe = data.DynamicPEValues.data,
            pb = data.PBValues.data,
            csvName = code + ".csv",
            csvPath = path.resolve(rPath, csvName);

        utils.existsFile(csvPath)
            .fail(function () {
                return Q.nfcall(fs.appendFile, csvPath, csvhead);
            })
            .then(function () {
                var lines = [];

                for (var i = 0; i < dates.length; i++) {
                    lines.push([code, name, dates[i], pe[i], dpe[i], pb[i]].join(","));
                }

                fs.appendFileSync(csvPath, "\r\n" + lines.join("\r\n"), "utf-8");
            });
    }
}