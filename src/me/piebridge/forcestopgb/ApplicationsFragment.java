package me.piebridge.forcestopgb;

import java.util.HashSet;
import java.util.Set;

import android.app.Activity;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;

public class ApplicationsFragment extends RefreshableListFragment {
	private MainActivity mActivity;
	private static Adapter mAdapter;

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		mActivity = (MainActivity) activity;
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		initAdapter();
	}

	@Override
	public void onDestroyView() {
		super.onDestroyView();
		setListAdapter(null);
	}

	@Override
	public void refresh(boolean force) {
		setListAdapter(null);
		if (force) {
			initAdapter();
		} else {
			setListAdapter(mAdapter);
		}
	}

	public void initAdapter() {
		mAdapter = new Adapter(mActivity);
		PackageManager pm = mActivity.getPackageManager();
		Set<String> names = new HashSet<String>();
		for (PackageInfo pkgInfo : pm.getInstalledPackages(0)) {
			ApplicationInfo appInfo = pkgInfo.applicationInfo;
			if (appInfo == null) {
				continue;
			}
			if (!appInfo.enabled) {
				continue;
			}
			if (appInfo.packageName.equals("android")) {
				continue;
			}
			names.add(appInfo.packageName);
		}
		mAdapter.addAll(names, true);
		setListAdapter(mAdapter);
	}

}