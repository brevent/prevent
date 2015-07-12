package me.piebridge.forcestopgb.ui;

import java.util.Set;

import me.piebridge.forcestopgb.R;

public class SettingFragmentPreventList extends SettingFragment {

    @Override
    protected Set<String> getPackageNames(SettingActivity activity) {
        return activity.getPreventPackages().keySet();
    }

    @Override
    protected boolean canUseCache() {
        return true;
    }

    @Override
    protected int getQueryHint() {
        return R.string.query_hint_system;
    }

}