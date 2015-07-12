package me.piebridge.forcestopgb;

import java.util.Set;

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

    private static Position position;

    @Override
    protected void setListPosition(Position _position) {
        position = _position;
    }

    @Override
    protected Position getListPosition() {
        return position;
    }


}