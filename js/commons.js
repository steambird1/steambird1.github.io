var dmain = new Database('asiagames', '01762bb6');
	
function update_all(update_list) {
	for (let it = 0; it < update_list.length; it++) {
		let i = update_list[it];
		let	tk = 't_' + i;
		dmain.getAsync(tk, function(data) {
			document.getElementById(i).innerHTML = data[tk];
		});
	}
}

function get_view(pid) {
	try {
		var dt = dmain.get("c_" + pid)["c_" + pid];
		if (dt == null || dt == "null") return 0;
		return parseInt(dt);
	} catch (e) {
		console.log(e);
		return 0;
	}
}

function update_view(pid) {
	dmain.getAsync("c_" + pid, function(data) {
		let dc = data["c_" + pid];
		if (dc == null || dc == "null") dc = "0";
		dmain.updateAsync("c_" + pid, parseInt(dc) + 1);
	});
}