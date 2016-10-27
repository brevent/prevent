package me.piebridge.prevent.ui;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.HandlerThread;
import android.provider.Settings;
import android.support.v4.view.ViewPager;
import android.text.TextUtils;
import android.util.TypedValue;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import me.piebridge.prevent.BuildConfig;
import me.piebridge.prevent.R;
import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.PreventListUtils;
import me.piebridge.prevent.ui.util.PreventUtils;
import me.piebridge.prevent.ui.util.ReportUtils;
import me.piebridge.prevent.ui.util.ThemeUtils;
import me.piebridge.prevent.ui.util.XposedUtils;

public class PreventActivity extends Activity implements ViewPager.OnPageChangeListener, View.OnClickListener {

    private ViewPager mPager;
    private String[] mPageTitles;
    private List<Set<String>> mPageSelections;

    private static Map<String, Boolean> preventPackages = null;
    private static Map<String, Set<Long>> running = new HashMap<String, Set<Long>>();

    private View main;
    private MenuItem removeMenu;
    private MenuItem preventMenu;

    private ProgressDialog dialog;

    private Integer dangerousColor = null;

    private Integer transparentColor = null;

    private BroadcastReceiver receiver;

    private Handler mHandler;

    private Handler mainHandler;

    private final Object preventLock = new Object();

    private boolean initialized;
    private boolean stopped;

    private int code;
    private String name;

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
        try {
            Class<?> clazz = Class.forName("de.robv.android.xposed.XposedBridge", false, ClassLoader.getSystemClassLoader());
            Field field = clazz.getDeclaredField("disableHooks");
            field.setAccessible(true);
            field.set(null, true);
            XposedUtils.disableXposed(clazz);
        } catch (Throwable t) { // NOSONAR
            // do nothing
        }
        ThemeUtils.setTheme(this);
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        ThemeUtils.fixSmartBar(this);

        mPager = (ViewPager) findViewById(R.id.pager);
        main = findViewById(R.id.main);
        findViewById(R.id.mismatch).setOnClickListener(this);
        receiver = new HookReceiver();

        mPageTitles = new String[] {getString(R.string.applications), getString(R.string.prevent_list)};
        mPageSelections = new ArrayList<Set<String>>();
        mPageSelections.add(new HashSet<String>());
        mPageSelections.add(new HashSet<String>());
        mPager.addOnPageChangeListener(this);
        mPager.setAdapter(new ScreenSlidePagerAdapter(getFragmentManager(), mPageTitles));

        HandlerThread thread = new HandlerThread("PreventUI");
        thread.start();
        mHandler = new Handler(thread.getLooper());
        mainHandler = new Handler(getMainLooper());

        if (PreventIntent.ACTION_NOT_SUPPORTED.equals(getIntent().getAction())) {
            reportBug();
        } else if (!BuildConfig.RELEASE) {
            showTestDialog();
        } else {
            initialize();
        }
    }

    private void initialize() {
        initialized = true;
        showProcessDialog(R.string.retrieving);
        mHandler.postDelayed(new Runnable() {
            @Override
            public void run() {
                if (!stopped) {
                    retrievePrevents();
                }
            }
        }, 0x100);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if (initialized) {
            mHandler.postDelayed(new Runnable() {
                @Override
                public void run() {
                    if (!stopped) {
                        retrievePrevents();
                    }
                }
            }, 0x400);
            mHandler.postDelayed(new Runnable() {
                @Override
                public void run() {
                    if (preventPackages == null && !stopped) {
                        showRetrieving();
                    }
                }
            }, 0x500);
        }
        mPager.getAdapter().notifyDataSetChanged();
    }

    private void showRetrieving() {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                synchronized (preventLock) {
                    if (preventPackages == null) {
                        showProcessDialog(R.string.retrieving);
                    }
                }
            }
        });
    }

    private void retrievePrevents() {
        PackageUtils.clearInputMethodPackages();
        Intent intent = new Intent();
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        intent.setAction(PreventIntent.ACTION_GET_PACKAGES);
        intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        UILog.i("sending get prevent packages broadcast");
        sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, receiver, mHandler, 0, null, null);
    }

    private void retrieveRunning() {
        if (name == null) {
            retrieveInfo();
        } else {
            showRebootIfNeeded();
        }
        Intent intent = new Intent();
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        intent.setAction(PreventIntent.ACTION_GET_PROCESSES);
        intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        UILog.i("sending get processes broadcast");
        sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, receiver, mHandler, 0, null, null);
    }

    private void retrieveInfo() {
        Intent intent = new Intent();
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        intent.setAction(PreventIntent.ACTION_GET_INFO);
        intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        UILog.i("sending get info broadcast");
        sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, receiver, mHandler, 0, null, null);
    }

    public Map<String, Set<Long>> getRunningProcesses() {
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
    public boolean onCreateOptionsMenu(Menu menu) {
        menu.clear();
        preventMenu = menu.add(Menu.NONE, R.string.prevent, Menu.NONE, R.string.prevent);
        preventMenu.setIcon(R.drawable.ic_menu_prevent);
        preventMenu.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        preventMenu.setVisible(false);
        removeMenu = menu.add(Menu.NONE, R.string.remove, Menu.NONE, R.string.remove);
        removeMenu.setIcon(R.drawable.ic_menu_recover);
        removeMenu.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        removeMenu.setVisible(false);
        menu.add(Menu.NONE, R.string.switch_theme, Menu.NONE, R.string.switch_theme);
        menu.add(Menu.NONE, R.string.report_bug, Menu.NONE, R.string.report_bug);
        menu.add(Menu.NONE, R.string.user_guide, Menu.NONE, R.string.user_guide);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();
        if (id == R.string.switch_theme) {
            return switchTheme();
        } else if (id == R.string.report_bug) {
            requestLog();
            return true;
        } else {
            return onClick(id);
        }
    }

    private boolean switchTheme() {
        ThemeUtils.switchTheme(this);
        dangerousColor = null;
        transparentColor = null;
        recreate();
        return true;
    }

    @Override
    public void onPageScrollStateChanged(int position) {
        // do nothing
    }

    @Override
    public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
        if (Float.floatToRawIntBits(positionOffset) == 0) {
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

    private boolean canPrevent(int position) {
        Set<String> selections = mPageSelections.get(position);
        return !selections.isEmpty() && !isSubSet(selections, getPreventPackages().keySet());
    }

    private boolean canRemove(int position) {
        Set<String> selections = mPageSelections.get(position);
        return !selections.isEmpty() && contains(selections, getPreventPackages().keySet());
    }

    private void checkSelection(int position) {
        if (preventMenu != null) {
            preventMenu.setVisible(canPrevent(position));
        }
        if (removeMenu != null) {
            removeMenu.setVisible(canRemove(position));
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

    private boolean contains(Set<String> a, Set<String> b) {
        for (String s : a) {
            if (b.contains(s)) {
                return true;
            }
        }
        return false;
    }

    public void changePrevent(String packageName, boolean prevent) {
        PreventUtils.update(this, new String[] {packageName}, prevent);
        if (prevent) {
            preventPackages.put(packageName, !running.containsKey(packageName));
        } else {
            preventPackages.remove(packageName);
        }
        savePackages();
    }

    private void savePackages() {
        refreshIfNeeded();
    }

    private void setUnchecked() {
        getCurrentFragment().setChecked(false);
    }

    @Override
    public void onClick(View v) {
        if (v.getId() == R.id.mismatch && code > 0) {
            PreventUtils.confirmReboot(this);
        } else {
            onClick(v.getId());
        }
    }

    private boolean onClick(int id) {
        int position = mPager.getCurrentItem();
        Set<String> selections = mPageSelections.get(position);
        if (id == R.string.prevent) {
            PreventUtils.update(this, selections.toArray(new String[selections.size()]), true);
            for (String packageName : selections) {
                preventPackages.put(packageName, !running.containsKey(packageName));
            }
            savePackages();
            setUnchecked();
        } else if (id == R.string.remove) {
            PreventUtils.update(this, selections.toArray(new String[selections.size()]), false);
            for (String packageName : selections) {
                preventPackages.remove(packageName);
            }
            savePackages();
            setUnchecked();
        } else if (id == R.string.user_guide) {
            startActivity(new Intent(this, UserGuideActivity.class));
        }
        selections.clear();
        checkSelection();
        return true;
    }

    @SuppressWarnings("deprecation")
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
        if (stopped) {
            return;
        }
        if (dialog == null) {
            dialog = new ProgressDialog(this);
        }
        dialog.setTitle(R.string.app_name);
        dialog.setIcon(R.drawable.ic_launcher);
        dialog.setCancelable(false);
        dialog.setMessage(getString(resId));
        dialog.show();
    }

    private boolean isInternal() {
        PackageManager pm = getPackageManager();
        try {
            String source = pm.getApplicationInfo(getPackageName(), 0).sourceDir;
            if (source.startsWith(Environment.getDataDirectory().getAbsolutePath()) || source.startsWith(Environment.getRootDirectory().getAbsolutePath())) {
                return true;
            }
        } catch (PackageManager.NameNotFoundException e) { // NOSONAR
            // do nothing
        }
        return false;
    }

    private void showDisableDialog(String result) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(getString(R.string.app_name) + "(" + BuildConfig.VERSION_NAME + ")");
        if (result == null) {
            builder.setMessage(getDisabledMessage());
        } else {
            builder.setMessage(result);
        }
        builder.setIcon(R.drawable.ic_launcher);
        builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
            @Override
            public void onCancel(DialogInterface dialog) {
                finish();
            }
        });
        builder.setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                fixDisabled();
            }
        });
        builder.setNeutralButton(R.string.report_bug, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                reportBug();
            }
        });
        builder.create().show();
    }

    private int getDisabledMessage() {
        if (!isInternal()) {
            return R.string.install_internal;
        } else {
            return R.string.no_patched;
        }
    }

    private void reportBug() {
        ReportUtils.reportBug(this);
    }

    private void fixDisabled() {
        if (!isInternal()) {
            startActivity(new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS, getPackage()));
        }
        finish();
    }

    private Uri getPackage() {
        return Uri.fromParts("package", getPackageName(), null);
    }

    private void showTestDialog() {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(getString(R.string.app_name) + "(" + BuildConfig.VERSION_NAME + ")");
        builder.setMessage(R.string.soak_version);
        builder.setIcon(R.drawable.ic_launcher);
        builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
            @Override
            public void onCancel(DialogInterface dialog) {
                finish();
            }
        });
        builder.setNegativeButton(android.R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                startActivity(new Intent(Intent.ACTION_DELETE, getPackage()));
                finish();
            }
        });
        builder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                initialize();
            }
        });
        builder.create().show();
    }

    private PreventFragment getCurrentFragment() {
        return getFragment(mPager.getCurrentItem());
    }

    private PreventFragment getFragment(int position) {
        return ((ScreenSlidePagerAdapter) mPager.getAdapter()).getFragment(position);
    }

    private boolean refresh(int position, boolean force) {
        PreventFragment fragment = getFragment(position);
        if (fragment != null) {
            fragment.saveListPosition();
            fragment.refresh(force);
            if (position == mPager.getCurrentItem()) {
                fragment.startTaskIfNeeded();
            }
            return true;
        } else {
            UILog.e("fragment is null in " + position);
            return false;
        }
    }

    private void refreshIfNeeded() {
        int position = mPager.getCurrentItem();
        int size = mPager.getAdapter().getCount();
        for (int item = 0; item < size; ++item) {
            if (item == position) {
                refresh(item, false);
            } else {
                refresh(item, true);
            }
        }
    }

    private void updateTimeIfNeeded(String packageName) {
        final PreventFragment fragment = getCurrentFragment();
        if (fragment != null) {
            fragment.updateTimeIfNeeded(packageName);
        }
    }

    private void notifyDataSetChanged() {
        final PreventFragment fragment = getCurrentFragment();
        if (fragment != null) {
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    fragment.notifyDataSetChanged();
                }
            });
        }
    }

    private boolean refresh(boolean force) {
        boolean showed = false;
        int size = mPager.getAdapter().getCount();
        for (int item = 0; item < size; ++item) {
            if (refresh(item, force)) {
                showed = true;
            }
        }
        return showed;
    }

    private boolean showRebootIfNeeded() {
        if (name == null) {
            return false;
        }
        if (BuildConfig.VERSION_NAME.equalsIgnoreCase(name)) {
            return false;
        }
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                findViewById(R.id.mismatch).setVisibility(View.VISIBLE);
            }
        });
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
            } else if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
                String packageName = PackageUtils.getPackageName(intent);
                if (running != null) {
                    running.remove(packageName);
                }
                if (preventPackages != null && Boolean.FALSE.equals(preventPackages.get(packageName))) {
                    preventPackages.put(packageName, true);
                }
                updateTimeIfNeeded(packageName);
            } else if (PreventIntent.ACTION_GET_INFO.equals(action)) {
                handleGetInfo(context);
            } else if (PreventIntent.ACTION_SYSTEM_LOG.equals(action)) {
                handleRequestLog();
            }
        }

        private void handleRequestLog() {
            ReportUtils.waitForCompleted(PreventActivity.this);
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    dialog.dismiss();
                    reportBug();
                }
            });
        }

        private void handleGetInfo(Context context) {
            String info = getResultData();
            if (TextUtils.isEmpty(info)) {
                return;
            }
            try {
                JSONObject json = new JSONObject(info);
                name = json.optString("name");
                code = json.optInt("code");
                PreventUtils.updateConfiguration(context, json);
                showRebootIfNeeded();
            } catch (JSONException e) {
                UILog.d("cannot get version from " + info, e);
            }
        }

        private void showFragments() {
            if (dialog != null && dialog.isShowing()) {
                runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        if (refresh(true)) {
                            dialog.dismiss();
                        } else {
                            showViewPager();
                            retrieveRunning();
                        }
                    }
                });
            }
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
                Map<String, Set<Long>> processes = new HashMap<String, Set<Long>>();
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
                notifyDataSetChanged();
            } catch (JSONException e) {
                UILog.e("cannot convert to json", e);
            }
        }

        private Set<Long> convertImportance(String value) {
            Set<Long> importance = new LinkedHashSet<Long>();
            for (String s : value.split(",")) {
                if (!TextUtils.isEmpty(s)) {
                    try {
                        importance.add(Long.parseLong(s));
                    } catch (NumberFormatException e) {
                        UILog.d("cannot format " + s, e);
                    }
                }
            }
            return importance;
        }

        private boolean handleGetPackages() {
            UILog.i("received get prevent packages broadcast");
            final String result = getResultData();
            if (result != null) {
                handlePackages(result);
                if (preventPackages != null) {
                    showViewPager();
                    retrieveRunning();
                    return true;
                }
            }
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    if (!stopped) {
                        showDisableDialog(result);
                    }
                }
            });
            return false;
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
                synchronized (preventLock) {
                    if (preventPackages == null) {
                        preventPackages = new HashMap<String, Boolean>();
                    } else {
                        preventPackages.clear();
                    }
                    preventPackages.putAll(prevents);
                }
                boolean synced = PreventListUtils.getInstance().syncIfNeeded(PreventActivity.this, prevents.keySet());
                if (synced) {
                    retrievePrevents();
                }
            } catch (JSONException e) {
                UILog.e("cannot convert to json: " + result, e);
            }
        }
    }


    @Override
    protected void onResume() {
        super.onResume();
        IntentFilter filter = new IntentFilter(Intent.ACTION_PACKAGE_RESTARTED);
        filter.addDataScheme("package");
        registerReceiver(receiver, filter);
        stopped = false;
        mainHandler.postDelayed(new Runnable() {
            @Override
            public void run() {
                if (!stopped) {
                    updateTimeIfNeeded(null);
                    mainHandler.postDelayed(this, 0x3e8);
                }
            }
        }, 0x3e8);
    }

    @Override
    public void onStop() {
        unregisterReceiver(receiver);
        preventPackages = null;
        super.onStop();
        stopped = true;
    }

    private void requestLog() {
        File dir = getExternalCacheDir();
        if (dir != null) {
            ReportUtils.clearReport(this);
            Intent intent = new Intent();
            intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
            intent.setAction(PreventIntent.ACTION_SYSTEM_LOG);
            intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
            UILog.i("sending request log broadcast");
            showProcessDialog(R.string.retrieving);
            sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, receiver, mHandler, 0, null, null);
        }
    }

}
