package me.piebridge.forcestopgb;

import java.io.File;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.content.Context;
import android.os.Bundle;
import android.os.FileUtils;
import android.view.View;
import android.widget.Button;
import android.widget.CompoundButton;

import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;

public class MainActivity extends FragmentActivity implements ViewPager.OnPageChangeListener, CompoundButton.OnCheckedChangeListener, View.OnClickListener {
	private ViewPager mPager;
	private String[] mPageTitles;
	private List<AbstractSet<String>> mPageSelections;;
	private PagerAdapter mPagerAdapter;
	private Object runningLock = new Object();
	private Object packageLock = new Object();
	private Map<String, Boolean> packages;
	private Map<String, Set<Integer>> running = new HashMap<String, Set<Integer>>();
	private Button remove;
	private Button cancel;
	private Button prevent;

	private static final int APPLICATIONS = 0;
	private static final int PREVENTLIST = 1;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		mPager = (ViewPager) findViewById(R.id.pager);
		mPageTitles = new String[] { getString(R.string.applications), getString(R.string.preventlist) };
		mPageSelections = new ArrayList<AbstractSet<String>>();
		mPageSelections.add(new HashSet<String>());
		mPageSelections.add(new HashSet<String>());
		mPagerAdapter = new ScreenSlidePagerAdapter(getSupportFragmentManager());
		mPager.setAdapter(mPagerAdapter);
		mPager.setOnPageChangeListener(this);
		remove = (Button) findViewById(R.id.remove);
		cancel = (Button) findViewById(R.id.cancel);
		prevent = (Button) findViewById(R.id.prevent);
		cancel.setOnClickListener(this);
		remove.setOnClickListener(this);
		prevent.setOnClickListener(this);
		cancel.setEnabled(false);
		prevent.setEnabled(false);
		remove.setEnabled(false);
		PackageProvider.ensureDirectory();
	}

	@Override
	protected void onResume() {
		super.onResume();
		packages = PackageProvider.loadFromFile(PackageProvider.FORCESTOP);
		new Thread(new Runnable() {
			@Override
			public void run() {
				synchronized (runningLock) {
					retrieveRunningProcesses();
				}
			}
		}).start();
	}

	private class ScreenSlidePagerAdapter extends FragmentPagerAdapter {

		public ScreenSlidePagerAdapter(FragmentManager fm) {
			super(fm);
		}

		@Override
		public Fragment getItem(int position) {
			switch (position) {
			case APPLICATIONS:
				return new ApplicationsFragment();
			case PREVENTLIST:
				return new PreventListFragment();
			default:
				return null;
			}
		}

		@Override
		public int getCount() {
			return mPageTitles.length;
		}

		@Override
		public CharSequence getPageTitle(int position) {
			return mPageTitles[position];
		}
	}

	private void retrieveRunningProcesses() {
		running.clear();
		ActivityManager manager = (ActivityManager) getSystemService(Context.ACTIVITY_SERVICE);
		List<RunningAppProcessInfo> processes = manager.getRunningAppProcesses();
		for (RunningAppProcessInfo process : processes) {
			for (String pkg : process.pkgList) {
				if (running.containsKey(pkg)) {
					running.get(pkg).add(process.importance);
				} else {
					Set<Integer> importance = new HashSet<Integer>();
					importance.add(process.importance);
					running.put(pkg, importance);
				}
			}
		}
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				refresh(APPLICATIONS, true);
				refresh(PREVENTLIST, true);
			}
		});
	}

	public Map<String, Set<Integer>> getRunningProcesses() {
		synchronized (runningLock) {
			return running;
		}
	}

	public Map<String, Boolean> getPackages() {
		synchronized (packageLock) {
			return packages;
		}
	}

	@Override
	public void onPageScrollStateChanged(int position) {
	}

	@Override
	public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
		if (positionOffset == 0) {
			if (position == APPLICATIONS) {
				prevent.setVisibility(View.VISIBLE);
				remove.setVisibility(View.GONE);
			} else {
				remove.setVisibility(View.VISIBLE);
				prevent.setVisibility(View.GONE);
			}
			checkSelection();
		}
	}

	@Override
	public void onPageSelected(int position) {
	}

	@Override
	public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
		ViewHolder holder = (ViewHolder) buttonView.getTag();
		Set<String> selections = getSelection();
		if (isChecked) {
			selections.add(holder.packageName);
		} else {
			selections.remove(holder.packageName);
		}
		checkSelection();
	}

	public Set<String> getSelection() {
		int position = mPager.getCurrentItem();
		return mPageSelections.get(position);
	}

	private void checkSelection() {
		Set<String> selections = getSelection();
		if (selections.size() > 0) {
			cancel.setEnabled(true);
			remove.setEnabled(true);
			if (isSubSet(selections, getPackages().keySet())) {
				prevent.setEnabled(false);
			} else {
				prevent.setEnabled(true);
			}
		} else {
			cancel.setEnabled(false);
			prevent.setEnabled(false);
			remove.setEnabled(false);
		}
	}

	private boolean isSubSet(Set<String> a, Set<String> b) {
		if (a.size() > b.size()) {
			return false;
		}
		for (String s : a) {
			if (!b.contains(s)) {
				return false;
			}
		}
		return true;
	}

	@Override
	public void onClick(View v) {
		int position = mPager.getCurrentItem();
		Set<String> selections = mPageSelections.get(position);
		switch (v.getId()) {
		case R.id.cancel:
			break;
		case R.id.prevent:
			synchronized (packageLock) {
				for (String packageName : selections) {
					if (!packages.containsKey(packageName)) {
						packages.put(packageName, !running.containsKey(packageName));
					}
				}
			}
			PackageProvider.saveToFile(PackageProvider.FORCESTOP, packages, "MainActivity");
			break;
		case R.id.remove:
			synchronized (packageLock) {
				for (String packageName : selections) {
					packages.remove(packageName);
				}
			}
			PackageProvider.saveToFile(PackageProvider.FORCESTOP, packages, "MainActivity");
			break;
		default:
			return;
		}
		selections.clear();
		refresh(APPLICATIONS, false);
		refresh(PREVENTLIST, false);
		checkSelection();
	}

	private void refresh(int position, boolean force) {
		((RefreshableListFragment) mPager.getAdapter().instantiateItem(mPager, position)).refresh(force);
	}
}
