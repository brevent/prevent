package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import org.json.JSONObject;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.util.HideApiUtils;
import me.piebridge.prevent.framework.util.HookUtils;
import me.piebridge.prevent.framework.util.LogUtils;

/**
 * Created by thom on 15/7/25.
 */
class SystemReceiver extends BroadcastReceiver {

    private static Map<String, Set<String>> abnormalProcesses = new ConcurrentHashMap<String, Set<String>>();

    private static Context mContext;

    public SystemReceiver(Context context) {
        mContext = context;
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        String packageName = PackageUtils.getPackageName(intent);
        if (PreventIntent.ACTION_GET_PACKAGES.equals(action)) {
            Map<String, Boolean> prevents = SystemHook.getPreventPackages();
            setResultCode(prevents.size());
            setResultData(new JSONObject(prevents).toString());
            LogUtils.logRequest(action, null, prevents.size());
            abortBroadcast();
        } else if (PreventIntent.ACTION_GET_PROCESSES.equals(action)) {
            Map<String, Set<Integer>> running = getRunningAppProcesses();
            LogUtils.logRequest(action, null, running.size());
            setResultData(toJSON(running));
            abortBroadcast();
        } else if (PreventIntent.ACTION_UPDATE_PREVENT.equals(action)) {
            handleUpdatePrevent(action, intent);
        } else if (PreventIntent.ACTION_INCREASE_COUNTER.equals(action)) {
            handleIncreaseCounter(action, packageName, intent);
        } else if (PreventIntent.ACTION_DECREASE_COUNTER.equals(action)) {
            handleDecreaseCounter(action, packageName, intent);
        } else if (PreventIntent.ACTION_RESTART.equals(action)) {
            handleRestart(action, packageName);
        } else if (PreventIntent.ACTION_ACTIVITY_DESTROY.equals(action)) {
            handleDestroy(action, packageName);
        } else if (PreventIntent.ACTION_FORCE_STOP.equals(action)) {
            handleForceStop(action, packageName, intent);
        }
    }

    private void handleUpdatePrevent(String action, Intent intent) {
        String[] packages = intent.getStringArrayExtra(PreventIntent.EXTRA_PACKAGES);
        boolean prevent = intent.getBooleanExtra(PreventIntent.EXTRA_PREVENT, true);
        Map<String, Boolean> prevents = SystemHook.getPreventPackages();
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

    private void handleIncreaseCounter(String action, String packageName, Intent intent) {
        if (SystemHook.getPreventPackages().containsKey(packageName)) {
            SystemHook.getPreventPackages().put(packageName, Boolean.FALSE);
        }
        int uid = intent.getIntExtra(PreventIntent.EXTRA_UID, 0);
        int pid = intent.getIntExtra(PreventIntent.EXTRA_PID, 0);
        setPid(pid, packageName);
        if (uid > 0) {
            SystemHook.getPackageUids().put(packageName, uid);
        }
        Map<Integer, AtomicInteger> packageCounter = SystemHook.getPackageCounters().get(packageName);
        if (packageCounter == null) {
            packageCounter = new HashMap<Integer, AtomicInteger>();
            SystemHook.getPackageCounters().put(packageName, packageCounter);
        }
        AtomicInteger pidCounter = packageCounter.get(pid);
        if (pidCounter == null) {
            pidCounter = new AtomicInteger();
            packageCounter.put(pid, pidCounter);
        }
        pidCounter.incrementAndGet();
        int count = countCounter(packageName);
        LogUtils.logRequest(action, packageName, count);
    }

    private void handleDecreaseCounter(String action, String packageName, Intent intent) {
        Map<Integer, AtomicInteger> packageCounter = SystemHook.getPackageCounters().get(packageName);
        if (packageCounter != null) {
            int pid = intent.getIntExtra(PreventIntent.EXTRA_PID, 0);
            AtomicInteger pidCounter = packageCounter.get(pid);
            if (pidCounter != null) {
                pidCounter.decrementAndGet();
            }
        }
        int count = countCounter(packageName);
        LogUtils.logRequest(action, packageName, count);
        if (count > 0) {
            return;
        }
        if (SystemHook.getPreventPackages().containsKey(packageName)) {
            SystemHook.getPreventPackages().put(packageName, Boolean.TRUE);
            LogUtils.logForceStop(action, packageName, "if needed in " + SystemHook.TIME_DESTROY + "s");
            SystemHook.checkRunningServices(packageName, SystemHook.TIME_DESTROY);
        } else {
            SystemHook.checkRunningServices(null, SystemHook.TIME_DESTROY);
        }
        SystemHook.killNoFather();
    }

    private void handleDestroy(String action, String packageName) {
        LogUtils.logRequest(action, packageName, -1);
        SystemHook.getPackageCounters().remove(packageName);
        if (SystemHook.getPreventPackages().containsKey(packageName)) {
            SystemHook.getPreventPackages().put(packageName, Boolean.TRUE);
            LogUtils.logForceStop(action, packageName, "destroy in " + SystemHook.TIME_SUICIDE + "s");
            SystemHook.forceStopPackageLater(packageName, SystemHook.TIME_SUICIDE);
        }
        SystemHook.checkRunningServices(null, SystemHook.TIME_SUICIDE < SystemHook.TIME_DESTROY ? SystemHook.TIME_DESTROY : SystemHook.TIME_SUICIDE);
        SystemHook.killNoFather();
    }

    private void handleRestart(String action, String packageName) {
        if (Boolean.TRUE.equals(SystemHook.getPreventPackages().get(packageName))) {
            SystemHook.getPreventPackages().put(packageName, Boolean.FALSE);
        }
        int count = countCounter(packageName);
        LogUtils.logRequest(action, packageName, count);
    }

    private void handleForceStop(String action, String packageName, Intent intent) {
        LogUtils.logRequest(action, packageName, -1);
        int uid = intent.getIntExtra(PreventIntent.EXTRA_UID, 0);
        int pid = intent.getIntExtra(PreventIntent.EXTRA_PID, 0);
        if (!shouldStop(packageName, pid)) {
            return;
        }
        SystemHook.getPackageCounters().remove(packageName);
        if (SystemHook.getPreventPackages().containsKey(packageName)) {
            SystemHook.getPreventPackages().put(packageName, Boolean.TRUE);
            LogUtils.logForceStop(action, packageName, "force in " + SystemHook.TIME_IMMEDIATE + "s" + ", uid: " + uid);
            SystemHook.forceStopPackageForce(packageName, SystemHook.TIME_IMMEDIATE);
        }
        SystemHook.checkRunningServices(null, SystemHook.TIME_IMMEDIATE < SystemHook.TIME_DESTROY ? SystemHook.TIME_DESTROY : SystemHook.TIME_IMMEDIATE);
    }

    private boolean shouldStop(String packageName, int pid) {
        countCounter(packageName);
        Map<Integer, AtomicInteger> values = SystemHook.getPackageCounters().get(packageName);
        if (values == null) {
            return true;
        }
        Set<Integer> pids = new HashSet<Integer>(values.keySet());
        pids.remove(pid);
        return pids.isEmpty();
    }

    private static String toJSON(Map<String, Set<Integer>> processes) {
        Map<String, String> results = new HashMap<String, String>();
        for (Map.Entry<String, Set<Integer>> entry : processes.entrySet()) {
            results.put(entry.getKey(), convertSet(entry.getValue()));
        }
        return new JSONObject(results).toString();
    }

    private static String convertSet(Set<Integer> value) {
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


    private static int countCounter(String packageName) {
        int count = 0;
        Map<Integer, AtomicInteger> values = SystemHook.getPackageCounters().get(packageName);
        if (values == null) {
            return count;
        }
        Iterator<Map.Entry<Integer, AtomicInteger>> iterator = values.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<Integer, AtomicInteger> entry = iterator.next();
            if (checkPid(entry.getKey(), packageName)) {
                count += entry.getValue().get();
            } else {
                LogUtils.logIgnore(entry.getKey(), packageName);
                iterator.remove();
            }
        }
        return count;
    }


    private static boolean checkPid(int pid, String packageName) {
        Integer uid = SystemHook.getPackageUids().get(packageName);
        if (uid == null) {
            return false;
        }
        try {
            if (HideApiUtils.getUidForPid(pid) != uid) {
                return false;
            }
        } catch (Throwable t) { // NOSONAR
            PreventLog.e("cannot get uid for " + pid, t);
        }
        String processName = SystemHook.getProcessName(pid);
        if (isNormalProcessName(processName, packageName)) {
            return true;
        }
        Set<String> abnormalPackages = abnormalProcesses.get(processName);
        return abnormalPackages != null && abnormalPackages.contains(packageName);
    }

    private static void setPid(int pid, String packageName) {
        String processName = SystemHook.getProcessName(pid);
        if (processName != null && !isNormalProcessName(processName, packageName)) {
            Set<String> abnormalProcess = abnormalProcesses.get(processName);
            if (abnormalProcess == null) {
                abnormalProcess = new HashSet<String>();
                abnormalProcesses.put(processName, abnormalProcess);
            }
            if (abnormalProcess.add(packageName)) {
                PreventLog.i("package " + packageName + " has abnormal process: " + processName);
            }
        }
    }

    private static boolean isNormalProcessName(String processName, String packageName) {
        return (processName != null) && (processName.equals(packageName) || processName.startsWith(packageName + ":"));
    }

    private static Map<String, Set<Integer>> getRunningAppProcesses() {
        Map<String, Set<Integer>> running = new HashMap<String, Set<Integer>>();
        Set<String> services = getRunningServices();
        ActivityManager activityManager = (ActivityManager) mContext.getSystemService(Context.ACTIVITY_SERVICE);
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
                } else if (services.contains(pkg)){
                    importance.add(process.importance);
                } else {
                    importance.add(-process.importance);
                }
            }
        }
        return running;
    }

    private static Set<String> getRunningServices() {
        Set<String> services = new HashSet<String>();
        for (ActivityManager.RunningServiceInfo service : HookUtils.getServices(mContext)) {
            if (service.started) {
                services.add(service.service.getPackageName());
            }
        }
        return services;
    }

}