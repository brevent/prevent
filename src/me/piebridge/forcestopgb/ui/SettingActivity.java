package me.piebridge.forcestopgb.ui;

import android.annotation.TargetApi;
import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.app.AlertDialog;
import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.app.FragmentUtils;
import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.util.TypedValue;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import me.piebridge.forcestopgb.R;
import me.piebridge.forcestopgb.common.CommonIntent;
import me.piebridge.forcestopgb.common.Packages;
import me.piebridge.util.RecreateUtil;

public class SettingActivity extends FragmentActivity implements ViewPager.OnPageChangeListener, View.OnClickListener {

    private ViewPager mPager;
    private PagerAdapter mPagerAdapter;
    private String[] mPageTitles;
    private List<Set<String>> mPageSelections;

    private final Object runningLock = new Object();
    private Map<String, Boolean> preventPackages = new HashMap<String, Boolean>();
    private Map<String, Set<Integer>> running = new HashMap<String, Set<Integer>>();
    private Map<String, Set<Integer>> backup = new HashMap<String, Set<Integer>>();
    private View main;
    private Button remove;
    private Button cancel;
    private Button prevent;

    private static final int APPLICATIONS = 0;
    private static final int PREVENTLIST = 1;

    private static final String THEME = "theme";
    private static final String THEME_LIGHT = "light";
    private static final String THEME_DARK = "dark";

    private AlertDialog dialog;

    private static Boolean hookEnabled = null;

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
            transparentColor = getResourceColor(android.R.color.transparent);
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
        main = findViewById(R.id.main);
        remove = (Button) findViewById(R.id.remove);
        cancel = (Button) findViewById(R.id.cancel);
        prevent = (Button) findViewById(R.id.prevent);
        cancel.setOnClickListener(this);
        remove.setOnClickListener(this);
        prevent.setOnClickListener(this);
        cancel.setEnabled(false);
        prevent.setEnabled(false);
        remove.setEnabled(false);
        mPagerAdapter = new ScreenSlidePagerAdapter(getSupportFragmentManager());
    }

    @Override
    protected void onResume() {
        super.onResume();
        showFragmentsIfNeeded();
    }


    private void showFragmentsIfNeeded() {
        if (hookEnabled == null || (Boolean.TRUE.equals(hookEnabled) && preventPackages.isEmpty())) {
            showAlertDialog(R.string.checking);
            Intent intent = new Intent();
            intent.setFlags(CommonIntent.INTENT_FLAG | Intent.FLAG_RECEIVER_FOREGROUND);
            intent.setAction(CommonIntent.ACTION_GET_PACKAGES);
            intent.setData(Uri.fromParts(CommonIntent.SCHEME, getPackageName(), null));
            Log.d(CommonIntent.TAG, "sending hook checking broadcast");
            sendOrderedBroadcast(intent, null, new HookReceiver(), null, 0, null, null);
        } else if (!hookEnabled) {
            showDisableDialog();
        } else {
            if (running.isEmpty()) {
                showAlertDialog(R.string.retrieving);
            }
            new Thread(new Runnable() {
                @Override
                public void run() {
                    retrieveRunningProcesses();
                }
            }).start();
        }
    }

    private void retrieveRunningProcesses() {
        backup.clear();
        ActivityManager manager = (ActivityManager) getSystemService(Context.ACTIVITY_SERVICE);
        List<RunningAppProcessInfo> processes = manager.getRunningAppProcesses();
        for (RunningAppProcessInfo process : processes) {
            for (String pkg : process.pkgList) {
                if (backup.containsKey(pkg)) {
                    backup.get(pkg).add(process.importance);
                } else {
                    Set<Integer> importance = new HashSet<Integer>();
                    importance.add(process.importance);
                    backup.put(pkg, importance);
                }
            }
        }
        synchronized (runningLock) {
            running.clear();
            running.putAll(backup);
        }
        backup.clear();
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                initFragmentIfNeeded();
                dialog.dismiss();
                refresh();
            }
        });
    }

    private void initFragmentIfNeeded() {
        if (mPager.getAdapter() == null) {
            mPageTitles = new String[]{getString(R.string.applications), getString(R.string.preventlist)};
            mPageSelections = new ArrayList<Set<String>>();
            mPageSelections.add(new HashSet<String>());
            mPageSelections.add(new HashSet<String>());
            mPager.addOnPageChangeListener(this);
            mPager.setAdapter(mPagerAdapter);
            main.setVisibility(View.VISIBLE);
        }
    }

    public Map<String, Set<Integer>> getRunningProcesses() {
        synchronized (runningLock) {
            return running;
        }
    }

    public Map<String, Boolean> getPreventPackages() {
        return preventPackages;
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

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == R.string.switch_theme) {
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
        } else {
            return false;
        }
    }

    @Override
    public void onPageScrollStateChanged(int position) {
        // do nothing
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
        if (!selections.isEmpty()) {
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
        PreventUtils.update(this, new String[]{packageName}, prevent);
        if (prevent) {
            preventPackages.put(packageName, !running.containsKey(packageName));
        } else {
            preventPackages.remove(packageName);
        }
        savePackages();
    }

    private void savePackages() {
        Packages.save(preventPackages.keySet());
        refreshIfNeeded();
    }

    @Override
    public void onClick(View v) {
        int id = v.getId();
        int position = mPager.getCurrentItem();
        Set<String> selections = mPageSelections.get(position);
        if (id == R.id.prevent) {
            PreventUtils.add(this, selections.toArray(new String[selections.size()]));
            for (String packageName : selections) {
                preventPackages.put(packageName, !running.containsKey(packageName));
            }
            savePackages();
        } else if (id == R.id.remove) {
            PreventUtils.remove(this, selections.toArray(new String[selections.size()]));
            for (String packageName : selections) {
                preventPackages.remove(packageName);
            }
            savePackages();
        }
        selections.clear();
        checkSelection();
    }

    public int getResourceColor(int colorId) {
        return getResources().getColor(colorId);
    }

    public int getThemed(int resId) {
        TypedValue tv = new TypedValue();
        getTheme().resolveAttribute(resId, tv, true);
        return tv.resourceId;
    }

    public int getThemedColor(int resId) {
        return getResourceColor(getThemed(resId));
    }

    private void showAlertDialog(int resId) {
        if (dialog == null) {
            AlertDialog.Builder builder = new AlertDialog.Builder(this);
            builder.setTitle(R.string.app_name);
            builder.setMessage(resId);
            builder.setIcon(R.drawable.ic_launcher);
            builder.setCancelable(false);
            dialog = builder.create();
        } else {
            dialog.setMessage(getString(resId));
        }
        dialog.show();
    }

    private void showDisableDialog() {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(R.string.app_name);
        builder.setMessage(R.string.app_notenabled);
        builder.setIcon(R.drawable.ic_launcher);
        builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
            @Override
            public void onCancel(DialogInterface dialog) {
                finish();
            }
        });
        builder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Intent intent = new Intent("de.robv.android.xposed.installer.OPEN_SECTION");
                intent.setPackage("de.robv.android.xposed.installer");
                intent.putExtra("section", "modules");
                intent.putExtra("module", getPackageName());
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                try {
                    startActivity(intent);
                } catch (ActivityNotFoundException e) { // NOSONAR
                    finish();
                }
            }
        });
        builder.create().show();
    }

    private void refresh(int position, boolean force) {
        String tag = mPageTitles[position];
        SettingFragment fragment = (SettingFragment) getSupportFragmentManager().findFragmentByTag(tag);
        if (fragment != null) {
            fragment.refresh(force || fragment.canUseCache());
        }
    }

    private void refreshIfNeeded() {
        int position = mPager.getCurrentItem();
        for (int item = 0; item < mPageTitles.length; ++item) {
            if (item != position) {
                refresh(item, false);
            }
        }
    }

    private void refresh() {
        for (int item = 0; item < mPageTitles.length; ++item) {
            refresh(item, true);
        }
    }

    private class HookReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.d(CommonIntent.TAG, "received hook checking broadcast");
            String result = getResultData();
            if (result != null) {
                hookEnabled = true;
                handlePackages(result);
            } else {
                hookEnabled = false;
            }
            // we are on main thread
            dialog.dismiss();
            showFragmentsIfNeeded();
        }

        private void handlePackages(String result) {
            try {
                JSONObject json = new JSONObject(result);
                preventPackages.clear();
                Iterator<String> it = json.keys();
                while (it.hasNext()) {
                    String key = it.next();
                    preventPackages.put(key, json.optBoolean(key));
                }
            } catch (JSONException e) { // NOSONAR
                // do nothing
                android.util.Log.d(CommonIntent.TAG, "cannot convert to json", e);
            }
        }
    }

    private class ScreenSlidePagerAdapter extends FragmentStatePagerAdapter {

        public ScreenSlidePagerAdapter(FragmentManager fm) {
            super(fm);
        }

        @Override
        public Fragment getItem(int position) {
            Fragment fragment;
            switch (position) {
                case APPLICATIONS:
                    fragment = new SettingFragmentApplications();
                    break;
                case PREVENTLIST:
                    fragment = new SettingFragmentPreventList();
                    break;
                default:
                    return null;
            }
            FragmentUtils.setTag(fragment, getPageTitle(position).toString());
            return fragment;
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


}
