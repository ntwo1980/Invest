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
    nconf = require("nconf"),
    utils = require("./utils"),
    home = process.env.HOME || process.env.USERPROFILE,
    download = home.replace(/\\/g, "/") + "/" + "Downloads/gz",
    rPath = path.resolve(__dirname, "../RInvest/gz"),
    configPath = path.resolve(__dirname, "../JsInvest/app.json"),
    csvhead = "Code,Name,Date,CompanyCount,SPE,RPE",
    variableCount = csvhead.split(",").length,
    days = [],
    today = moment(),
    endDate = today,
    config = {};

exports.getData = function () {
    nconf.file({ file: configPath });

    date = nconf.get("gz:date"),
        //config.date = date ? date : "2016-08-01";
        config.date = date ? date : "2009-10-29";

    var startDate = moment(config.date).add(1, "days"),
        day = startDate,
        dayDiff = day.diff(endDate, "days");

    if (dayDiff == 0) {
        console.log("done");
        return;
    }

    while (dayDiff <= 0) {
        days.push(moment(day));

        if (dayDiff == 0)
            break;

        day.add(1, "days");
        dayDiff = day.diff(endDate, "days");
    }

    var commands = _.map(days, limit(function (day) {
        var dayStr = day.format("YYYY-MM-DD"),
            url = "http://www.cnindex.com.cn/syl/" + dayStr + "/cninfo_hsls.html";

        console.log("processing " + dayStr);

        return utils.getUrl(url).then(function (body) {
            var document = $(body),
                dailyData = {
                    date: dayStr,
                    data: [],
                };

            if (document.length != 22)
                throw "document length is not 22";

            var content = $($(document[21]).html());

            if (content.length != 7)
                throw "content length is not 7";

            var tbl = $(content[2]);

            if (!tbl.hasClass("table_01_box"))
                throw "wrong table";

            _.forEach(tbl.find("tr"), function (tr) {
                var tds = $(tr).find("td"),
                    lis = $(tds[0]).find("ul li"),
                    code = _.reduce(lis, function (code, li) {
                        var txt = _.trim($(li).text());

                        if (txt != "")
                            code = txt;

                        return code;
                    }, ""),
                    name = _.trim($(tds[1]).text()),
                    companyCount = _.trim($(tds[2]).text()),
                    spe = _.trim($(tds[3]).text()),
                    rpe = _.trim($(tds[5]).text());

                var record = {
                    date: dayStr,
                    code: code,
                    name: name,
                    companyCount: companyCount,
                    spe: spe == "NA" ? "" : spe,
                    rpe: rpe == "NA" ? "" : rpe
                };

                dailyData.data.push(record);
            });

            return dailyData;

        }, function (eror) { });
    }));

    Q.all(commands).done(function (data) {
        var data = _.reject(data, function (item) {
            return !item;
        }), stocks = {};

        if (!data.length)
            return;

        data = _.sortBy(data, function (r) { return r.date; });

        _.forEach(data, function (r) {
            _.forEach(r.data, function (dailyData) {
                pushStockDailyData(stocks, dailyData);
            });
        });

        _.forEach(_.keys(stocks), function (code) {
            processStock(code, stocks[code]);
        });

        //console.log(stocks);
        nconf.set('gz:date', endDate.format("YYYY-MM-DD"));
        nconf.save();

        console.log("done");
    });

    function pushStockDailyData(stocks, dailyData) {
        if (!stocks[dailyData.code])
            stocks[dailyData.code] = [];

        stocks[dailyData.code].push(dailyData);
    }

    function processStock(code, data) {
        var csvName = code + ".csv",
            csvPath = path.resolve(rPath, csvName);

        utils.existsFile(csvPath)
            .fail(function () {
                return Q.nfcall(fs.appendFile, csvPath, csvhead);
            })
            .then(function () {
                var lines = [];

                _.forEach(data, function (item) {
                    lines.push([item.code, item.name, item.date, item.companyCount, item.spe, item.rpe].join(","));
                });

                fs.appendFileSync(csvPath, "\r\n" + lines.join("\r\n"), "utf-8");
            });
    }
}
