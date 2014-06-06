package me.piebridge.forcestopgb;

import java.text.Collator;
import java.util.Set;

import android.content.pm.ApplicationInfo;

public class AppInfo implements Comparable<AppInfo> {
	String packageName = "";
	String name = "";
	Set<Integer> running = null;
	int flags;

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
		return (running == null ? "1" : "0") + (isSystem() ? "1" : "0") + "/" + name + "/" + packageName;
	}

	@Override
	public int compareTo(AppInfo another) {
		return Collator.getInstance().compare(toString(), another.toString());
	}

	public AppInfo flags(int _flags) {
		flags = _flags;
		return this;
	}

	public boolean isSystem() {
		return (flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
	}

}