package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;

import org.json.JSONObject;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import me.piebridge.forcestopgb.BuildConfig;
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
            PreventIntent.ACTION_REQUEST_LOG
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
            if (!BuildConfig.RELEASE) {
                handleNonScheme(action);
            }
        }
    }

    private void handleManager(Context context, Intent intent, String action) {
        if (PreventIntent.ACTION_GET_PACKAGES.equals(action)) {
            handleGetPackages(action);
        } else if (PreventIntent.ACTION_GET_PROCESSES.equals(action)) {
            handleGetProcesses(context, action);
        } else if (PreventIntent.ACTION_UPDATE_PREVENT.equals(action)) {
            handleUpdatePrevent(action, intent);
        } else if (PreventIntent.ACTION_REQUEST_LOG.equals(action)) {
            LogcatUtils.logcat();
            int size = (int) LogcatUtils.logcat(context);
            setResultCode(size);
        }
    }

    private void handlePackage(Intent intent, String action) {
        String packageName = PackageUtils.getPackageName(intent);
        if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
            handlePackageRestarted("PACKAGE_RESTARTED", packageName);
            onPackageRestarted(packageName);
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
        Map<String, Set<Integer>> running = getRunningAppProcesses(context);
        LogUtils.logRequest(action, null, running.size());
        setResultData(toJSON(running));
        abortBroadcast();
    }

    private void handleGetPackages(String action) {
        Map<String, Boolean> preventPackages = new HashMap<String, Boolean>(mPreventPackages);
        Boolean canStopGms = GmsUtils.canStopGms();
        if (!canStopGms) {
            for (String packageName : GmsUtils.getGmsPackages()) {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    preventPackages.put(packageName, false);
                }
            }
        }
        int size = preventPackages.size();
        setResultCode(size);
        setResultData(new JSONObject(preventPackages).toString());
        LogUtils.logRequest(action, null, size);
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
        LogUtils.logRequest(action, null, prevents.size());
        abortBroadcast();
    }

    private String toJSON(Map<String, Set<Integer>> processes) {
        Map<String, String> results = new HashMap<String, String>();
        for (Map.Entry<String, Set<Integer>> entry : processes.entrySet()) {
            results.put(entry.getKey(), convertSet(entry.getValue()));
        }
        return new JSONObject(results).toString();
    }

    private String convertSet(Set<Integer> value) {
        StringBuilder sb = new StringBuilder();
        Iterator<Integer> it = value.iterator();
        while (it.hasNext()) {
            Integer v = it.next();
            if (v != null) {
                sb.append(v);
                if (it.hasNext()) {
                    sb.append(",");
                }
            }
        }
        return sb.toString();
    }

    private Map<String, Set<Integer>> getRunningAppProcesses(Context context) {
        Map<String, Set<Integer>> running = new HashMap<String, Set<Integer>>();
        Set<String> services = getRunningServices(context);
        ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        List<ActivityManager.RunningAppProcessInfo> processes = activityManager.getRunningAppProcesses();
        if (processes == null) {
            PreventLog.w("cannot get running processes");
            return running;
        }
        for (ActivityManager.RunningAppProcessInfo process : processes) {
            for (String pkg : process.pkgList) {
                Set<Integer> importance = running.get(pkg);
                if (importance == null) {
                    importance = new HashSet<Integer>();
                    running.put(pkg, importance);
                }
                if (process.importance != ActivityManager.RunningAppProcessInfo.IMPORTANCE_SERVICE) {
                    importance.add(process.importance);
                } else if (services.contains(pkg)) {
                    importance.add(process.importance);
                } else {
                    importance.add(-process.importance);
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
