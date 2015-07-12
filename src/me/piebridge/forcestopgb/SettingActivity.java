package me.piebridge.forcestopgb;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import me.piebridge.util.RecreateUtil;

import android.annotation.TargetApi;
import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.content.Context;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.TypedValue;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;

import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;

public class SettingActivity extends FragmentActivity implements ViewPager.OnPageChangeListener, View.OnClickListener {

    private ViewPager mPager;
    private String[] mPageTitles;
    private List<AbstractSet<String>> mPageSelections;

    private final Object runningLock = new Object();
    private final Object packageLock = new Object();
    private Map<String, Boolean> preventPackages;
    private Map<String, Set<Integer>> running = new HashMap<String, Set<Integer>>();
    private Button remove;
    private Button cancel;
    private Button prevent;

    private static final int APPLICATIONS = 0;
    private static final int PREVENTLIST = 1;

    private Integer dangerousColor = null;

    private Integer transparentColor = null;

    public int getDangerousColor() {
        if (dangerousColor == null) {
            dangerousColor = getThemedColor(R.attr.color_dangerous);
        }
        return dangerousColor;
    }

    public int getTransparentColor() {
        if (transparentColor == null) {
            transparentColor = getColor(android.R.color.transparent);
        }
        return transparentColor;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        final SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        setTheme(THEME_LIGHT.equals(sp.getString(THEME, THEME_LIGHT)) ? R.style.light : R.style.dark);
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        mPager = (ViewPager) findViewById(R.id.pager);
        mPageTitles = new String[]{getString(R.string.applications), getString(R.string.preventlist)};
        mPageSelections = new ArrayList<AbstractSet<String>>();
        mPageSelections.add(new HashSet<String>());
        mPageSelections.add(new HashSet<String>());
        PagerAdapter mPagerAdapter = new ScreenSlidePagerAdapter(getSupportFragmentManager());
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
    }

    @Override
    protected void onResume() {
        super.onResume();
        getPreventPackages();
        new Thread(new Runnable() {
            @Override
            public void run() {
                synchronized (runningLock) {
                    retrieveRunningProcesses();
                }
                try {
                    Thread.sleep(250);
                } catch (InterruptedException e) {
                    // do nothing
                }
                runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        getPreventPackages();
                        refresh();
                    }
                });
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
                    return new SettingFragmentApplications();
                case PREVENTLIST:
                    return new SettingFragmentPreventList();
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
    }

    public Map<String, Set<Integer>> getRunningProcesses() {
        synchronized (runningLock) {
            return running;
        }
    }

    private long mtime;

    public Map<String, Boolean> getPreventPackages() {
        synchronized (packageLock) {
            long time = PreventPackages.lastModified();
            if (preventPackages == null || mtime < time) {
                preventPackages = PreventPackages.load();
                mtime = time;
            }
            return preventPackages;
        }
    }

    @Override
    @TargetApi(Build.VERSION_CODES.HONEYCOMB)
    public boolean onCreateOptionsMenu(Menu menu) {
        menu.clear();
        MenuItem item = menu.add(Menu.NONE, R.string.switch_theme, Menu.NONE, R.string.switch_theme);
        if (Build.VERSION.SDK_INT > Build.VERSION_CODES.GINGERBREAD_MR1) {
            item.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        }
        return super.onCreateOptionsMenu(menu);
    }

    private static final String THEME = "theme";
    private static final String THEME_LIGHT = "light";
    private static final String THEME_DARK = "dark";

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.string.switch_theme:
                final SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
                String theme = sp.getString(THEME, THEME_LIGHT);
                if (THEME_LIGHT.equals(theme)) {
                    sp.edit().putString(THEME, THEME_DARK).apply();
                } else {
                    sp.edit().putString(THEME, THEME_LIGHT).apply();
                }
                dangerousColor = null;
                transparentColor = null;
                RecreateUtil.recreate(this);
                return true;
            default:
                return false;
        }
    }

    @Override
    public void onPageScrollStateChanged(int position) {
    }

    @Override
    public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
        if (positionOffset == 0) {
            checkSelection(position);
        }
    }

    @Override
    public void onPageSelected(int position) {
        checkSelection(position);
    }

    public Set<String> getSelection() {
        return mPageSelections.get(mPager.getCurrentItem());
    }

    public void checkSelection() {
        checkSelection(mPager.getCurrentItem());
    }

    private void checkSelection(int position) {
        if (position == APPLICATIONS) {
            prevent.setVisibility(View.VISIBLE);
            remove.setVisibility(View.GONE);
        } else {
            remove.setVisibility(View.VISIBLE);
            prevent.setVisibility(View.GONE);
        }
        Set<String> selections = mPageSelections.get(position);
        if (selections.size() > 0) {
            cancel.setEnabled(true);
            remove.setEnabled(true);
            if (isSubSet(selections, getPreventPackages().keySet())) {
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

    public void changePrevent(String packageName, boolean prevent) {
        String action = prevent ? SystemHook.ADD_PREVENT_PACKAGE : SystemHook.REMOVE_PREVENT_PACKAGE;
        this.sendBroadcast(Hook.newIntent(action, packageName, null));
        synchronized (packageLock) {
            if (prevent) {
                preventPackages.put(packageName, !running.containsKey(packageName));
            } else {
                preventPackages.remove(packageName);
            }
        }
        refreshIfNeeded(false);
    }

    private void refresh(int position, boolean force) {
        SettingFragment fragment = (SettingFragment) mPager.getAdapter().instantiateItem(mPager, position);
        fragment.refresh(force || fragment.canUseCache());
    }

    public void showFilter() {
        for (int item = 0; item < mPageTitles.length; ++item) {
            SettingFragment fragment = (SettingFragment) mPager.getAdapter().instantiateItem(mPager, item);
            fragment.showFilter();
        }
    }

    private void refreshIfNeeded(boolean force) {
        int position = mPager.getCurrentItem();
        for (int item = 0; item < mPageTitles.length; ++item) {
            if (item != position) {
                refresh(item, force);
            } else if (force) {
                refresh(item, false);
            }
        }
    }

    private void refresh() {
        for (int item = 0; item < mPageTitles.length; ++item) {
            refresh(item, true);
        }
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
                    Iterator<String> it = selections.iterator();
                    while (it.hasNext()) {
                        String packageName = it.next();
                        this.sendBroadcast(Hook.newIntent(SystemHook.ADD_PREVENT_PACKAGE, packageName, it.hasNext() ? SystemHook.DELAY_SAVE : null));
                        if (!preventPackages.containsKey(packageName)) {
                            preventPackages.put(packageName, !running.containsKey(packageName));
                        }
                    }
                }
                break;
            case R.id.remove:
                synchronized (packageLock) {
                    Iterator<String> it = selections.iterator();
                    while (it.hasNext()) {
                        String packageName = it.next();
                        this.sendBroadcast(Hook.newIntent(SystemHook.REMOVE_PREVENT_PACKAGE, packageName, it.hasNext() ? SystemHook.DELAY_SAVE : null));
                        preventPackages.remove(packageName);
                    }
                }
                break;
            default:
                return;
        }
        selections.clear();
        refreshIfNeeded(true);
        checkSelection();
    }

    public int getColor(int colorId) {
        return getResources().getColor(colorId);
    }

    public int getThemed(int resId) {
        TypedValue tv = new TypedValue();
        getTheme().resolveAttribute(resId, tv, true);
        return tv.resourceId;
    }

    public int getThemedColor(int resId) {
        return getColor(getThemed(resId));
    }

}
