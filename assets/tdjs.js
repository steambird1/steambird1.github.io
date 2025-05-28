// tdjs.js
// Tiny Database JavaScript Support
// (c) 2022 seabird (3587320341@qq.com)

// JavaScript support for
const prefix_api = "https://tinywebdb.appinventor.space/api";
const db_limit = 100;

function DatabaseException(msg) {
	
}

function internalParse(hq) {
		var res = hq.responseText;
		if ((res.length <= 0) || (res[0] != "{")) {
			throw new DatabaseException(res);
		} else {
			return res;
		}
	};

function ifdef(judge, if_def, if_ndef) {
	if (judge == undefined) {
		return if_ndef;
	} else {
		return if_def;
	}
}

function Database(username, passwd) {
	
	this.getResponseTextSync = function(request_appendix) {
		var hq = new XMLHttpRequest();
		hq.open("POST", prefix_api + "?user=" + this.username + "&secret=" + this.passwd + "&" + request_appendix, false);
		hq.send(null);
		return internalParse(hq);
	};
	
	// async receiver must be a function like
	// function receiver (data).
	// data will be a dictionary (for 'get' and 'search') or value (for 'length'/'count').
	
	// Internal receiver contains XMLHttpRequest object.
	this.getResponseTextAsync = function(request_appendix, internal_receiver) {
		var hq = new XMLHttpRequest();
		hq.onreadystatechange = function () {
			if (hq.readyState == 4) {
				//console.log(hq.responseText);
				internal_receiver(hq);
			}
		}
		hq.open("POST", prefix_api + "?user=" + this.username + "&secret=" + this.passwd + "&" + request_appendix, true);
		hq.send(null);
	};
	
	this.length = function() {
		return parseInt(JSON.parse(this.getResponseTextSync("action=count"))["count"]);
	};
	
	this.lengthAsync = function(receiver) {
		this.getResponseTextAsync("action=count", function(hq) {
			receiver(parseInt(JSON.parse(internalParse(hq))["count"]));
		});
	};
	
	this.update = function(key, value) {
		this.getResponseTextSync("action=update&tag=" + key + "&value=" + value);
	};
	
	// receiver() have no argument.
	this.updateAsync = function(key, value, receiver) {
		this.getResponseTextAsync("action=update&tag=" + key + "&value=" + value, function(hq) { if (receiver != undefined) receiver(); });
	};
	
	this.remove = function(key) {
		this.getResponseTextSync("action=delete&tag=" + key);
	}
	
	this.removeAsync = function(key, receiver) {
		this.getResponseTextAsync("action=delete&tag=" + key, function(hq) { if (receiver != undefined) receiver(); });
	}
	
	this.get = function(key) {
		return JSON.parse(this.getResponseTextSync("action=get&tag=" + key));
	};
	
	this.getAsync = function(key, receiver) {
		this.getResponseTextAsync("action=get&tag=" + key, function(hq) {
			receiver(JSON.parse(internalParse(hq)));
		});
	};
	
	this.searchSingle = function(tag, start, count) {
		return JSON.parse(this.getResponseTextSync("action=search" + ifdef(tag, "&tag=" + tag, "") + ifdef(start, "&no=" + start, "") + ifdef(count, "&count=" + count, "")));
	}
	
	this.searchSingleAsync = function(receiver, tag, start, count) {
		this.getResponseTextAsync("action=search" + ifdef(tag, "&tag=" + tag, "") + ifdef(start, "&no=" + start, "") + ifdef(count, "&count=" + count, ""), function(hq) {
			receiver(JSON.parse(internalParse(hq)));
		});
	}
	
	this.search = function(tag, start, count) {
		var temp = new Object();
		var tl = this.length();
		var starter = start;
		var counter = count;
		if (start == null) starter = 1;
		if (count == null) counter = tl;
		for (let i = starter; i <= counter; i += db_limit) {
			Object.assign(temp, this.searchSingle(tag, i, db_limit));
		}
		return temp;
	}

	this.searchAsync = function(tag, receiver, start, count, retry_cont, extra) {
		if (retry_cont == null) retry_cont = 5;
		var logging = false;
		if (retry_cont < 0) {
			logging = true;
			retry_cont = -retry_cont;
		}
		var tl = 0;
		var ts = this, err = null;
		for (let i = 0; i < retry_cont; i++) {
			try {
				tl = this.length();
				err = null;
				break;
			} catch (e) {
				err = e;
				if (logging) console.log("[tdjs] Error in async search initialization: "+e+" ("+(i+1)+"/"+retry_cont+" attempts)");
			}
		}
		if (err != null) throw err;
		var starter = start;
		var counter = count;
		if (start == null) starter = 1;
		if (count == null) counter = tl;
		if (extra == null) extra = new Object();
		
		if (counter <= 0) {
			receiver(extra);
			return;
		}
		
		for (let i = 0; i < retry_cont; i++) {
			try {
				this.searchSingleAsync(function(cur) {
					Object.assign(extra, cur);
					ts.searchAsync(tag, receiver, starter+db_limit, counter-db_limit, retry_cont, extra);
				}, tag, start, db_limit);
				err = null;
				break;
			} catch (e) {
				err = e;
				if (logging) console.log("[tdjs] Error in async search: "+e+" ("+(i+1)+"/"+retry_cont+" attempts)");
			}
		}
		if (err != null) throw err;
	}
	
	this.username = username;
	this.passwd = passwd;
	this.length();			// Check user name and password.
}