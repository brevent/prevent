package me.piebridge.forcestopgb;

import java.text.Collator;
import java.util.Set;

public class AppInfo implements Comparable<AppInfo> {
	String packageName = "";
	String name = "";
	Set<Integer> running = null;

	public AppInfo(String _packageName, String _name, Set<Integer> _running) {
		super();
		packageName = _packageName;
		if (_name != null) {
			name = _name;
		}
		running = _running;
	}

	@Override
	public String toString() {
		return (running != null ? "0" : "1") + "/" + name + "/" + packageName;
	}

	@Override
	public int compareTo(AppInfo another) {
		return Collator.getInstance().compare(toString(), another.toString());
	}

}