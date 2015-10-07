package me.piebridge.prevent.framework;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.telephony.TelephonyManager;

import org.json.JSONObject;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.util.HookUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.LogcatUtils;
import me.piebridge.prevent.framework.util.SafeActionUtils;

/**
 * Created by thom on 15/7/25.
 */
public class SystemReceiver extends ActivityReceiver {

    public static final Collection<String> MANAGER_ACTIONS = Arrays.asList(
            PreventIntent.ACTION_GET_PACKAGES,
            PreventIntent.ACTION_GET_PROCESSES,
            PreventIntent.ACTION_UPDATE_PREVENT,
            PreventIntent.ACTION_SYSTEM_LOG,
            PreventIntent.ACTION_UPDATE_TIMEOUT,
            PreventIntent.ACTION_CHECK_LICENSE
    );

    public static final Collection<String> PACKAGE_ACTIONS = Arrays.asList(
            Intent.ACTION_PACKAGE_RESTARTED,
            Intent.ACTION_PACKAGE_ADDED,
            Intent.ACTION_PACKAGE_REMOVED
    );

    public static final Collection<String> NON_SCHEME_ACTIONS = Arrays.asList(
            Intent.ACTION_SCREEN_OFF,
            Intent.ACTION_SCREEN_ON
    );

    public SystemReceiver(Context context, Map<String, Boolean> preventPackages) {
        super(context, preventPackages);
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        if (MANAGER_ACTIONS.contains(action)) {
            handleManager(context, intent, action);
        } else if (PACKAGE_ACTIONS.contains(action)) {
            handlePackage(intent, action);
        } else if (NON_SCHEME_ACTIONS.contains(action)) {
            handleNonScheme(action);
        }
    }

    private void handleManager(Context context, Intent intent, String action) {
        if (PreventIntent.ACTION_GET_PACKAGES.equals(action)) {
            handleGetPackages(action);
        } else if (PreventIntent.ACTION_GET_PROCESSES.equals(action)) {
            handleGetProcesses(context, action);
        } else if (PreventIntent.ACTION_UPDATE_PREVENT.equals(action)) {
            handleUpdatePrevent(action, intent);
        } else if (PreventIntent.ACTION_SYSTEM_LOG.equals(action)) {
            LogcatUtils.logcat("*:v");
            LogcatUtils.logcat(context, "system");
            LogcatUtils.logcat("-s Prevent:v PreventUI:v");
            setResultCode((int) LogcatUtils.logcat(context, "prevent"));
        } else if (PreventIntent.ACTION_UPDATE_TIMEOUT.equals(action)) {
            timeout = intent.getLongExtra(PreventIntent.EXTRA_TIMEOUT, -1);
            PreventLog.i("update timeout to " + timeout + "s");
        } else if (PreventIntent.ACTION_CHECK_LICENSE.equals(action)) {
            handleCheckLicense(context, intent);
        }
    }

    private boolean handleCheckLicense(Context context, Intent intent) {
        String user = intent.getStringExtra(Intent.EXTRA_USER);
        Map<String, String> users = new LinkedHashMap<String, String>();
        for (Account account : AccountManager.get(context).getAccounts()) {
            users.put(account.type, account.name);
            if (PackageUtils.equals(account.name, user)) {
                setResultCode(0x1);
                return true;
            }
        }
        String number = ((TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE)).getLine1Number();
        if (number != null) {
            number = number.replace("-", "");
            number = number.replace(" ", "");
            users.put("line1Number", number);
            if (PackageUtils.equals(number, user)) {
                setResultCode(0x1);
                return true;
            }
        }
        setResultCode(0x0);
        setResultData(users.toString());
        return false;
    }

    private void handlePackage(Intent intent, String action) {
        String packageName = PackageUtils.getPackageName(intent);
        if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
            handlePackageRestarted("PACKAGE_RESTARTED", packageName);
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(action)) {
            SafeActionUtils.onPackageChanged(packageName);
        } else if (Intent.ACTION_PACKAGE_REMOVED.equals(action)) {
            SafeActionUtils.onPackageChanged(packageName);
            onPackageRemoved(packageName);
        }
    }

    private void handleNonScheme(String action) {
        if (Intent.ACTION_SCREEN_OFF.equals(action)) {
            onScreenOff();
        } else if (Intent.ACTION_SCREEN_ON.equals(action)) {
            onScreenOn();
        }
    }

    private void handleGetProcesses(Context context, String action) {
        Map<String, Set<Long>> running = getRunningAppProcesses(context);
        Map<String, Long> leaving = getLeavingPackages();
        for (Map.Entry<String, Long> entry : leaving.entrySet()) {
            String packageName = entry.getKey();
            Set<Long> status = running.get(packageName);
            if (status != null) {
                long elapsed = entry.getValue();
                if (elapsed % 0xa == 0) {
                    elapsed += 1;
                }
                status.add(elapsed);
            }
        }
        LogUtils.logRequestInfo(action, null, running.size());
        setResultData(toJSON(running));
        abortBroadcast();
    }

    private void handleGetPackages(String action) {
        Map<String, Boolean> preventPackages = new TreeMap<String, Boolean>(mPreventPackages);
        if (!GmsUtils.canStopGms()) {
            for (String packageName : GmsUtils.getGmsPackages()) {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    preventPackages.put(packageName, false);
                }
            }
        }
        int size = preventPackages.size();
        setResultCode(size);
        setResultData(new JSONObject(preventPackages).toString());
        LogUtils.logRequestInfo(action, null, size);
        abortBroadcast();
    }

    private void handleUpdatePrevent(String action, Intent intent) {
        String[] packages = intent.getStringArrayExtra(PreventIntent.EXTRA_PACKAGES);
        boolean prevent = intent.getBooleanExtra(PreventIntent.EXTRA_PREVENT, true);
        Map<String, Boolean> prevents = mPreventPackages;
        for (String name : packages) {
            if (prevent) {
                int count = countCounter(name);
                prevents.put(name, count == 0);
            } else {
                prevents.remove(name);
            }
        }
        setResultCode(prevents.size());
        setResultData(new JSONObject(prevents).toString());
        LogUtils.logRequestInfo(action, null, prevents.size());
        abortBroadcast();
    }

    private String toJSON(Map<String, Set<Long>> processes) {
        Map<String, String> results = new HashMap<String, String>();
        for (Map.Entry<String, Set<Long>> entry : processes.entrySet()) {
            results.put(entry.getKey(), convertSet(entry.getValue()));
        }
        return new JSONObject(results).toString();
    }

    private String convertSet(Set<Long> value) {
        StringBuilder sb = new StringBuilder();
        Iterator<Long> it = value.iterator();
        while (it.hasNext()) {
            Long v = it.next();
            if (v != null) {
                sb.append(v);
                if (it.hasNext()) {
                    sb.append(",");
                }
            }
        }
        return sb.toString();
    }

    private Map<String, Set<Long>> getRunningAppProcesses(Context context) {
        Map<String, Set<Long>> running = new HashMap<String, Set<Long>>();
        Set<String> services = getRunningServices(context);
        ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        List<ActivityManager.RunningAppProcessInfo> processes = activityManager.getRunningAppProcesses();
        if (processes == null) {
            PreventLog.w("cannot get running processes");
            return running;
        }
        for (ActivityManager.RunningAppProcessInfo process : processes) {
            for (String pkg : process.pkgList) {
                Set<Long> importance = running.get(pkg);
                if (importance == null) {
                    importance = new LinkedHashSet<Long>();
                    running.put(pkg, importance);
                }
                if (process.importance != ActivityManager.RunningAppProcessInfo.IMPORTANCE_SERVICE) {
                    importance.add((long) process.importance);
                } else if (services.contains(pkg)) {
                    importance.add((long) process.importance);
                } else {
                    importance.add((long) -process.importance);
                }
            }
        }
        return running;
    }

    private Set<String> getRunningServices(Context context) {
        Set<String> services = new HashSet<String>();
        for (ActivityManager.RunningServiceInfo service : HookUtils.getServices(context)) {
            if (service.started) {
                services.add(service.service.getPackageName());
            }
        }
        return services;
    }

    private void handlePackageRestarted(String action, String packageName) {
        LogUtils.logRequestInfo(action, packageName, -1);
        removePackageCounters(packageName);
        SystemHook.updateRunningGapps(packageName, false);
        if (mPreventPackages.containsKey(packageName)) {
            mPreventPackages.put(packageName, true);
        }
        SystemHook.killNoFather();
    }

}
