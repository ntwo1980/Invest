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
    limit = qlimit(5),
    moment = require("moment"),
    nconf = require("nconf"),
    utils = require("./utils"),
    home = process.env.HOME || process.env.USERPROFILE,
    rPath = path.resolve(__dirname, "../RInvest/zz"),
    configPath = path.resolve(__dirname, "../JsInvest/app.json"),
    csvhead = "Code,Date,PE,RPE,PB",
    variableCount = csvhead.split(",").length,
    days = [],
    today = moment(),
    endDate = today,
    config = {},
    codes = [  
        //"000001",  //bank
        //"002142",
        //"600000",
        //"600015",
        //"600016",
        //"600036",
        //"601009",
        //"601166",
        //"601169",
        //"601288",
        //"601328",
        //"601398",
        //"601818",
        //"601939",
        //"601988",
        //"601998",  
        //"600837"    // security
        //"600030"
    ];

exports.getData = function () {
    nconf.file({ file: configPath });

    date = nconf.get("zz:date"),
        //config.date = date ? date : "2016-08-23";
        config.date = date ? date : "2012-01-01";

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

    _.forEach(codes, function (code) {
        var commands = _.map(days, limit(function (day) {
            var dayStr = day.format("YYYY-MM-DD"),
                url = "http://www.csindex.com.cn/sseportal/csiportal/syl/indexsyl.do?type=2&indexCode=" + code + "&date=" + dayStr + "&classType=1";

            console.log("processing " + dayStr);

            return utils.getUrl(url).then(function (body) {
                var document = $(body),
                    dailyData = [
                    ];

                if (document.length != 31)
                    throw "document length is not 31";

                var content = $(document[29]),
                    tbl = content.find("table.list-table"),
                    tr = $(tbl.find("tr")[1]),
                    tds = tr.children(),
                    pe = _.trim($(tds[7]).text()),
                    rpe = _.trim($(tds[8]).text()),
                    pb = _.trim($(tds[9]).text()),
                    pay = _.trim($(tds[10]).text());

               return {
                    code: code,
                    date: dayStr,
                    pe: pe,
                    rpe: rpe,
                    pb: pb,
                    pay: pay
                };
            }, function (eror) { });
        }));

        Q.all(commands).done(function (data) {
            var data = _.reject(data, function (item) {
                return !item.pe;
            }), stocks = {};

            if (!data.length)
                return;

            data = _.sortBy(data, function (r) {
                return r.date;
            });

            processStock(data);

            ////console.log(stocks);
            //nconf.set('zz:date', endDate.format("YYYY-MM-DD"));
            //nconf.save();

            console.log("done");
        });
    });
    //function pushStockDailyData(stocks, dailyData) {
    //    if (!stocks[dailyData.code])
    //        stocks[dailyData.code] = [];

    //    stocks[dailyData.code].push(dailyData);
    //}

    function processStock(data) {
        if (!data.length)
            return;

        var csvName = data[0].code + ".csv",
            csvPath = path.resolve(rPath, csvName);

        utils.existsFile(csvPath)
            .fail(function () {
                return Q.nfcall(fs.appendFile, csvPath, csvhead);
            })
            .then(function () {
                var lines = [];

                _.forEach(data, function (item) {
                    lines.push([item.code, item.date, item.pe, item.rpe, item.pb].join(","));
                });

                fs.appendFileSync(csvPath, "\r\n" + lines.join("\r\n"), "utf-8");
            });
    }
}
