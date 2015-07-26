package me.piebridge.prevent.ui;

import android.annotation.TargetApi;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.HandlerThread;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.app.FragmentUtils;
import android.support.v4.view.ViewPager;
import android.text.TextUtils;
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
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.PreventListUtils;
import me.piebridge.prevent.ui.util.PreventUtils;
import me.piebridge.prevent.ui.util.RecreateUtils;

public class PreventActivity extends FragmentActivity implements ViewPager.OnPageChangeListener, View.OnClickListener {

    private ViewPager mPager;
    private String[] mPageTitles;
    private List<Set<String>> mPageSelections;

    private static Map<String, Boolean> preventPackages = null;
    private static Map<String, Set<Integer>> running = new HashMap<String, Set<Integer>>();

    private View main;
    private Button remove;
    private Button cancel;

    private Button prevent;

    private static final int APPLICATIONS = 0;
    private static final int PREVENT_LIST = 1;

    private static final String THEME = "theme";
    private static final String THEME_LIGHT = "light";
    private static final String THEME_DARK = "dark";

    private ProgressDialog dialog;

    private Integer dangerousColor = null;

    private Integer transparentColor = null;

    private BroadcastReceiver receiver;

    private Handler mHandler;

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
        receiver = new HookReceiver();

        mPageTitles = new String[]{getString(R.string.applications), getString(R.string.preventlist)};
        mPageSelections = new ArrayList<Set<String>>();
        mPageSelections.add(new HashSet<String>());
        mPageSelections.add(new HashSet<String>());
        mPager.addOnPageChangeListener(this);
        mPager.setAdapter(new ScreenSlidePagerAdapter(getSupportFragmentManager()));

        HandlerThread thread = new HandlerThread("PreventUI");
        thread.start();
        mHandler = new Handler(thread.getLooper());
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (preventPackages == null) {
            showProcessDialog(R.string.retrieving);
            mHandler.postDelayed(new Runnable() {
                @Override
                public void run() {
                    retrievePrevents();
                }
            }, 0x100);
        }
    }

    private void retrievePrevents() {
        Intent intent = new Intent();
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        intent.setAction(PreventIntent.ACTION_GET_PACKAGES);
        intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        UILog.i("sending get prevent packages broadcast");
        sendOrderedBroadcast(intent, null, receiver, mHandler, 0, null, null);
    }

    private void retrieveRunning() {
        Intent intent = new Intent();
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        intent.setAction(PreventIntent.ACTION_GET_PROCESSES);
        intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        UILog.i("sending get processes broadcast");
        sendOrderedBroadcast(intent, null, receiver, mHandler, 0, null, null);
    }

    public Map<String, Set<Integer>> getRunningProcesses() {
        return running;
    }

    public Map<String, Boolean> getPreventPackages() {
        if (preventPackages == null) {
            return new HashMap<String, Boolean>();
        } else {
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
            RecreateUtils.recreate(this);
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
        refresh(position, false);
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
        PreventListUtils.save(preventPackages.keySet());
        refreshIfNeeded();
    }

    @Override
    public void onClick(View v) {
        int id = v.getId();
        int position = mPager.getCurrentItem();
        Set<String> selections = mPageSelections.get(position);
        if (id == R.id.prevent) {
            PreventUtils.update(this, selections.toArray(new String[selections.size()]), true);
            for (String packageName : selections) {
                preventPackages.put(packageName, !running.containsKey(packageName));
            }
            savePackages();
        } else if (id == R.id.remove) {
            PreventUtils.update(this, selections.toArray(new String[selections.size()]), false);
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

    private void showProcessDialog(int resId) {
        if (dialog == null) {
            dialog = new ProgressDialog(this);
        }
        dialog.setTitle(R.string.app_name);
        dialog.setIcon(R.drawable.ic_launcher);
        dialog.setCancelable(false);
        dialog.setMessage(getString(resId));
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

    private boolean refresh(int position, boolean force) {
        String tag = mPageTitles[position];
        int currentItem = mPager.getCurrentItem();
        PreventFragment fragment = (PreventFragment) getSupportFragmentManager().findFragmentByTag(tag);
        if (fragment != null) {
            fragment.saveListPosition();
            fragment.refresh(force);
            if (position == currentItem) {
                fragment.startTaskIfNeeded();
            }
            return true;
        } else {
            return false;
        }
    }

    private void refreshIfNeeded() {
        int position = mPager.getCurrentItem();
        for (int item = mPageTitles.length - 1; item >= 0; --item) {
            if (item == position) {
                refresh(item, false);
            } else {
                refresh(item, true);
            }
        }
    }

    private boolean refresh(boolean force) {
        for (int item = mPageTitles.length - 1; item >= 0; --item) {
            if (!refresh(item, force)) {
                return false;
            }
        }
        return true;
    }

    private class HookReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            if (PreventIntent.ACTION_GET_PROCESSES.equals(action)) {
                handleGetProcesses();
                showFragments();
            } else if (PreventIntent.ACTION_GET_PACKAGES.equals(action)) {
                handleGetPackages();
            }
        }

        private void showFragments() {
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    dialog.dismiss();
                    while (!refresh(true)) {
                        retrieveRunning();
                    }
                }
            });
        }

        private void handleGetProcesses() {
            UILog.i("received get processes broadcast");
            String result = getResultData();
            if (result != null) {
                handleProcesses(result);
            }
        }

        private void handleProcesses(String result) {
            try {
                JSONObject json = new JSONObject(result);
                Map<String, Set<Integer>> processes = new HashMap<String, Set<Integer>>();
                Iterator<String> it = json.keys();
                while (it.hasNext()) {
                    String key = it.next();
                    String value = json.optString(key);
                    if (value != null) {
                        processes.put(key, convertImportance(value));
                    }
                }
                running.clear();
                running.putAll(processes);
            } catch (JSONException e) {
                UILog.e("cannot convert to json", e);
            }
        }

        private Set<Integer> convertImportance(String value) {
            Set<Integer> importance = new HashSet<Integer>();
            for (String s : value.split(",")) {
                if (!TextUtils.isEmpty(s) && TextUtils.isDigitsOnly(s)) {
                    importance.add(Integer.parseInt(s));
                }
            }
            return importance;
        }

        private void handleGetPackages() {
            UILog.i("received get prevent packages broadcast");
            String result = getResultData();
            if (result != null) {
                showViewPager();
                handlePackages(result);
                retrieveRunning();
            } else {
                runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        showDisableDialog();
                    }
                });
            }
        }

        private void showViewPager() {
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    main.setVisibility(View.VISIBLE);
                }
            });
        }

        private void handlePackages(String result) {
            try {
                JSONObject json = new JSONObject(result);
                Map<String, Boolean> prevents = new HashMap<String, Boolean>();
                Iterator<String> it = json.keys();
                while (it.hasNext()) {
                    String key = it.next();
                    prevents.put(key, json.optBoolean(key));
                }
                if (preventPackages == null) {
                    preventPackages = new HashMap<String, Boolean>();
                } else {
                    preventPackages.clear();
                }
                preventPackages.putAll(prevents);
            } catch (JSONException e) {
                UILog.e("cannot convert to json", e);
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
                    fragment = new PreventFragment.Applications();
                    break;
                case PREVENT_LIST:
                    fragment = new PreventFragment.PreventList();
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


    @Override
    public void onPause() {
        preventPackages = null;
        super.onPause();
    }

}
