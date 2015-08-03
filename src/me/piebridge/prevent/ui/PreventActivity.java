package me.piebridge.prevent.ui;

import android.annotation.TargetApi;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
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
import java.util.Objects;
import java.util.Set;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
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

    private final Object preventLock = new Object();

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
        showProcessDialog(R.string.retrieving);
        mHandler.postDelayed(new Runnable() {
            @Override
            public void run() {
                retrievePrevents();
            }
        }, 0x100);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if (preventPackages == null) {
            mHandler.post(new Runnable() {
                @Override
                public void run() {
                    retrievePrevents();
                }
            });
            mHandler.postDelayed(new Runnable() {
                @Override
                public void run() {
                    if (preventPackages == null) {
                        showRetrieving();
                    }
                }
            }, 0x100);
        }
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
        Intent intent = new Intent();
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        intent.setAction(PreventIntent.ACTION_GET_PACKAGES);
        intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        UILog.i("sending get prevent packages broadcast");
        sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, receiver, mHandler, 0, null, null);
    }

    private void retrieveRunning() {
        Intent intent = new Intent();
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        intent.setAction(PreventIntent.ACTION_GET_PROCESSES);
        intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        UILog.i("sending get processes broadcast");
        sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, receiver, mHandler, 0, null, null);
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
        menu.add(Menu.NONE, R.string.switch_theme, Menu.NONE, R.string.switch_theme);
        if (BuildConfig.WECHAT_DONATE && getDonateAlipay() != null) {
            menu.add(Menu.NONE, R.string.donate_alipay, Menu.NONE, R.string.donate_alipay);
        }
        if (BuildConfig.ALIPAY_DONATE && getDonateWeChat() != null) {
            menu.add(Menu.NONE, R.string.donate_wechat, Menu.NONE, R.string.donate_wechat);
            menu.add(Menu.NONE, R.string.donate_wechat_lucky, Menu.NONE, R.string.donate_wechat_lucky);
        }
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();
        if (BuildConfig.WECHAT_DONATE && (id == R.string.donate_wechat || id == R.string.donate_wechat_lucky)) {
            return donateViaWeChat(id);
        } else if (BuildConfig.ALIPAY_DONATE && id == R.string.donate_alipay) {
            return donateViaAlipay();
        } else {
            return id == R.string.switch_theme && switchTheme();
        }
    }

    private boolean switchTheme() {
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
    }

    private ComponentName getDonateWeChat() {
        return getDonateComponent(PreventIntent.NAME_WECHAT, PreventIntent.CLASS_WECHAT);
    }

    private ComponentName getDonateAlipay() {
        return getDonateComponent(PreventIntent.NAME_ALIPAY, PreventIntent.CLASS_ALIPAY);
    }

    private ComponentName getDonateComponent(String packageName, String className) {
        ComponentName cn = new ComponentName(packageName, className);
        try {
            PackageManager pm = getPackageManager();
            ActivityInfo ai = pm.getActivityInfo(cn, 0);
            int enabled = pm.getComponentEnabledSetting(cn);
            if (BuildConfig.DEBUG) {
                UILog.d("exported: " + ai.exported + ", enabled: " + ai.enabled + ", component enabled: " + enabled);
            }
            if (!ai.exported) {
                return null;
            }
            if (ai.enabled && enabled == PackageManager.COMPONENT_ENABLED_STATE_DEFAULT) {
                return cn;
            }
            if (enabled == PackageManager.COMPONENT_ENABLED_STATE_ENABLED) {
                return cn;
            }
        } catch (PackageManager.NameNotFoundException e) { // NOSONAR
            UILog.d("cannot find " + packageName + "/" + className);
        }
        return null;
    }

    private boolean donateViaWeChat(int id) {
        ComponentName cn = getDonateWeChat();
        if (cn == null) {
            return false;
        }
        Intent intent = new Intent();
        intent.setComponent(cn);
        intent.putExtra("scene", 1);
        if (id == R.string.donate_wechat_lucky) {
            intent.putExtra("receiver_name", BuildConfig.WECHAT_ACCOUNT + "&s=37");
        } else {
            intent.putExtra("receiver_name", BuildConfig.WECHAT_ACCOUNT);
        }
        try {
            startActivity(intent);
        } catch (Throwable t) { // NOSONAR
            // do nothing
        }
        return true;
    }

    private boolean donateViaAlipay() {
        ComponentName cn = getDonateAlipay();
        if (cn == null) {
            return false;
        }
        Intent intent = new Intent();
        intent.setComponent(cn);
        intent.putExtra("app_id", "20000053");
        Bundle mExtras = new Bundle();
        mExtras.putString("bizData", BuildConfig.ALIPAY_ACCOUNT);
        intent.putExtra("mExtras", mExtras);
        try {
            startActivity(intent);
        } catch (Throwable t) { // NOSONAR
            // do nothing
        }
        return true;
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
        builder.setTitle(getString(R.string.app_name) + "(" + BuildConfig.VERSION_NAME + ")");
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
            UILog.e("fragment is null in " + position);
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
                    if (!refresh(true)) {
                        showViewPager();
                        retrieveRunning();
                    } else {
                        dialog.dismiss();
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
                UILog.d("result: " + result);
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
                if (!TextUtils.isEmpty(s)) {
                    try {
                        importance.add(Integer.parseInt(s));
                    } catch (NumberFormatException e) {
                        UILog.d("cannot format " + s, e);
                    }
                }
            }
            return importance;
        }

        private boolean handleGetPackages() {
            UILog.i("received get prevent packages broadcast");
            String result = getResultData();
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
                    showDisableDialog();
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
    public void onStop() {
        preventPackages = null;
        super.onStop();
    }

}
