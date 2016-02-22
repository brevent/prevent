package me.piebridge.prevent.common;

import android.os.Bundle;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Created by thom on 16/2/21.
 */
public class Configuration {

    private long forceStopTimeout = -1;
    private boolean destroyProcesses;
    private boolean backupPreventList;
    private boolean lockSyncSettings;
    private boolean autoPrevent;
    private boolean stopSignatureApps;
    private boolean useAppStandby;

    private final Map<String, Object> map;

    public Configuration(Bundle bundle) {
        map = new LinkedHashMap<String, Object>();
        setBackupPreventList(bundle.getBoolean(PreventIntent.KEY_BACKUP_PREVENT_LIST));
        setDestroyProcesses(bundle.getBoolean(PreventIntent.KEY_DESTROY_PROCESSES));
        setForceStopTimeout(bundle.getLong(PreventIntent.KEY_FORCE_STOP_TIMEOUT));
        setLockSyncSettings(bundle.getBoolean(PreventIntent.KEY_LOCK_SYNC_SETTINGS));
        setAutoPrevent(bundle.getBoolean(PreventIntent.KEY_AUTO_PREVENT, true));
        if (bundle.containsKey(PreventIntent.KEY_STOP_SIGNATURE_APPS)) {
            setStopSignatureApps(bundle.getBoolean(PreventIntent.KEY_STOP_SIGNATURE_APPS, true));
        }
        setUseAppStandby(bundle.getBoolean(PreventIntent.KEY_USE_APP_STANDBY));
    }

    public long getForceStopTimeout() {
        return forceStopTimeout;
    }

    public boolean isDestroyProcesses() {
        return destroyProcesses;
    }

    public boolean isBackupPreventList() {
        return backupPreventList;
    }

    public boolean isLockSyncSettings() {
        return lockSyncSettings;
    }

    public boolean isAutoPrevent() {
        return autoPrevent;
    }

    public boolean isStopSignatureApps() {
        return stopSignatureApps;
    }

    public boolean isUseAppStandby() {
        return useAppStandby;
    }

    public void setForceStopTimeout(long forceStopTimeout) {
        this.forceStopTimeout = forceStopTimeout;
        this.map.put(PreventIntent.KEY_FORCE_STOP_TIMEOUT, forceStopTimeout);
    }

    public void setDestroyProcesses(boolean destroyProcesses) {
        this.destroyProcesses = destroyProcesses;
        this.map.put(PreventIntent.KEY_DESTROY_PROCESSES, destroyProcesses);
    }

    public void setBackupPreventList(boolean backupPreventList) {
        this.backupPreventList = backupPreventList;
        this.map.put(PreventIntent.KEY_BACKUP_PREVENT_LIST, backupPreventList);
    }

    public void setAutoPrevent(boolean autoPrevent) {
        this.autoPrevent = autoPrevent;
        this.map.put(PreventIntent.KEY_AUTO_PREVENT, autoPrevent);
    }

    public void setLockSyncSettings(boolean lockSyncSettings) {
        this.lockSyncSettings = lockSyncSettings;
        this.map.put(PreventIntent.KEY_LOCK_SYNC_SETTINGS, lockSyncSettings);
    }

    public void setStopSignatureApps(boolean stopSignatureApps) {
        this.stopSignatureApps = stopSignatureApps;
        this.map.put(PreventIntent.KEY_STOP_SIGNATURE_APPS, stopSignatureApps);
    }

    public void setUseAppStandby(boolean useAppStandby) {
        this.useAppStandby = useAppStandby;
        this.map.put(PreventIntent.KEY_USE_APP_STANDBY, useAppStandby);
    }

    public Map<String, Object> getMap() {
        return map;
    }
}
