package me.piebridge.prevent.framework;

import android.content.BroadcastReceiver;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.prevent.framework.util.ActivityRecordUtils;
import me.piebridge.prevent.framework.util.HideApiUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.ProcessRecordUtils;

abstract class ActivityReceiver extends BroadcastReceiver {

    protected Map<String, Boolean> mPreventPackages;

    private Map<String, Integer> packageUids = new HashMap<String, Integer>();
    private Map<String, Set<String>> abnormalProcesses = new ConcurrentHashMap<String, Set<String>>();
    private Map<String, Map<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, Map<Integer, AtomicInteger>>();

    public ActivityReceiver(Map<String, Boolean> preventPackages) {
        mPreventPackages = preventPackages;
    }

    protected int countCounter(String packageName) {
        return countCounter(-1, packageName);
    }

    private int countCounter(int currentPid, String packageName) {
        int count = 0;
        Map<Integer, AtomicInteger> values = packageCounters.get(packageName);
        if (values == null) {
            return count;
        }
        Iterator<Map.Entry<Integer, AtomicInteger>> iterator = values.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<Integer, AtomicInteger> entry = iterator.next();
            int pid = entry.getKey();
            if (pid == currentPid || checkPid(pid, packageName)) {
                count += entry.getValue().get();
            } else {
                LogUtils.logIgnore(entry.getKey(), packageName);
                iterator.remove();
            }
        }
        return count;
    }


    private boolean checkPid(int pid, String packageName) {
        Integer uid = packageUids.get(packageName);
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

    private boolean isNormalProcessName(String processName, String packageName) {
        return (processName != null) && (processName.equals(packageName) || processName.startsWith(packageName + ":"));
    }

    private void setAbnormalProcessIfNeeded(String processName, String packageName) {
        if (!isNormalProcessName(processName, packageName)) {
            Set<String> abnormalProcess = abnormalProcesses.get(processName);
            if (abnormalProcess == null) {
                abnormalProcess = new HashSet<String>();
                abnormalProcesses.put(processName, abnormalProcess);
            }
            if (abnormalProcess.add(packageName)) {
                PreventLog.d("package " + packageName + " has abnormal process: " + processName);
            }
        }
    }

    public void onStartActivity(Object activityRecord) {
        String packageName = ActivityRecordUtils.getPackageName(activityRecord);
        SystemHook.cancelCheck(packageName);
        SystemHook.updateRunningGapps(packageName, true);
        if (mPreventPackages.containsKey(packageName)) {
            mPreventPackages.put(packageName, false);
        }
        int pid = ActivityRecordUtils.getPid(activityRecord);
        int uid = ActivityRecordUtils.getUid(activityRecord);
        String processName = ActivityRecordUtils.getInfo(activityRecord).processName;
        setAbnormalProcessIfNeeded(processName, packageName);
        if (uid > 0) {
            packageUids.put(packageName, uid);
        }
        Map<Integer, AtomicInteger> packageCounter = packageCounters.get(packageName);
        if (packageCounter == null) {
            packageCounter = new HashMap<Integer, AtomicInteger>();
            packageCounters.put(packageName, packageCounter);
        }
        AtomicInteger pidCounter = packageCounter.get(pid);
        if (pidCounter == null) {
            pidCounter = new AtomicInteger();
            packageCounter.put(pid, pidCounter);
        }
        pidCounter.incrementAndGet();
        int count = countCounter(pid, packageName);
        LogUtils.logActivity("start activity", packageName, count);
    }

    public void onDestroyActivity(Object activityRecord) {
        String packageName = ActivityRecordUtils.getPackageName(activityRecord);
        Map<Integer, AtomicInteger> packageCounter = packageCounters.get(packageName);
        if (packageCounter != null) {
            int pid = ActivityRecordUtils.getPid(activityRecord);
            AtomicInteger pidCounter = packageCounter.get(pid);
            if (pidCounter != null) {
                pidCounter.decrementAndGet();
            }
        }
        int count = countCounter(packageName);
        LogUtils.logActivity("destroy activity", packageName, count);
        if (count > 0) {
            return;
        }
        SystemHook.updateRunningGapps(packageName, false);
        if (mPreventPackages.containsKey(packageName)) {
            mPreventPackages.put(packageName, true);
            LogUtils.logForceStop("destroy activity", packageName, "if needed in " + SystemHook.TIME_DESTROY + "s");
            SystemHook.checkRunningServices(packageName, SystemHook.TIME_DESTROY);
        } else {
            SystemHook.checkRunningServices(null, SystemHook.TIME_DESTROY);
        }
        SystemHook.killNoFather();
    }

    public void onDestroyActivity(String reason, String packageName) {
        SystemHook.updateRunningGapps(packageName, false);
        if (mPreventPackages.containsKey(packageName)) {
            mPreventPackages.put(packageName, true);
            LogUtils.logForceStop(reason, packageName, "destroy in " + SystemHook.TIME_SUICIDE + "s");
            SystemHook.checkRunningServices(packageName, SystemHook.TIME_SUICIDE);
        } else {
            SystemHook.checkRunningServices(null, SystemHook.TIME_SUICIDE < SystemHook.TIME_DESTROY ? SystemHook.TIME_DESTROY : SystemHook.TIME_SUICIDE);
        }
        SystemHook.killNoFather();
    }

    public void onResumeActivity(Object activityRecord) {
        String packageName = ActivityRecordUtils.getPackageName(activityRecord);
        SystemHook.cancelCheck(packageName);
        SystemHook.updateRunningGapps(packageName, true);
        if (Boolean.TRUE.equals(mPreventPackages.get(packageName))) {
            mPreventPackages.put(packageName, false);
        }
        int count = countCounter(packageName);
        LogUtils.logActivity("resume activity", packageName, count);
    }

    public void onAppDied(Object processRecord) {
        String packageName = ProcessRecordUtils.getInfo(processRecord).packageName;
        int count = countCounter(packageName);
        LogUtils.logActivity("app died", packageName, count);
        int pid = ProcessRecordUtils.getPid(processRecord);
        if (!shouldStop(packageName, pid)) {
            return;
        }
        SystemHook.updateRunningGapps(packageName, false);
        if (mPreventPackages.containsKey(packageName)) {
            mPreventPackages.put(packageName, true);
            SystemHook.checkRunningServices(packageName, SystemHook.TIME_IMMEDIATE < SystemHook.TIME_DESTROY ? SystemHook.TIME_DESTROY : SystemHook.TIME_IMMEDIATE);
        }
    }

    private boolean shouldStop(String packageName, int pid) {
        countCounter(packageName);
        Map<Integer, AtomicInteger> values = packageCounters.get(packageName);
        if (values == null) {
            return true;
        }
        Set<Integer> pids = new HashSet<Integer>(values.keySet());
        pids.remove(pid);
        return pids.isEmpty();
    }

    protected void removePackageCounters(String packageName) {
        packageCounters.remove(packageName);
    }

}
